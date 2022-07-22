use std::convert::TryInto;

use account::AccountWithReward;
use std::collections::HashMap;
use near_sdk::borsh::{self, BorshDeserialize, BorshSerialize};
use near_sdk::collections::{UnorderedMap, UnorderedSet, Vector};
use near_sdk::json_types::U128;
use near_sdk::serde::{Deserialize, Serialize};
use near_sdk::{
    env, ext_contract, near_bindgen, AccountId, Balance, BorshStorageKey, EpochHeight, Gas,
    Promise, PromiseResult, PublicKey, log,
};
use crate::staking_pool::Fraction;
use uint::construct_uint;

use crate::account::{NumStakeShares, Account};
use crate::farm::Farm;
use crate::internal::ZERO_ADDRESS;
use crate::staking_pool::{InnerStakingPool, InnerStakingPoolWithoutRewardsRestaked};
pub use crate::views::{HumanReadableAccount, HumanReadableFarm, PoolSummary, ContractBalances, ExpectedTokensInFuture};

mod staking_pool;
mod account;
mod farm;
mod internal;
mod owner;
mod stake;
#[cfg(test)]
mod test_utils;
mod token_receiver;
mod views;

/// The amount of gas given to complete internal `on_stake_action` call.
const ON_STAKE_ACTION_GAS: Gas = Gas(20_000_000_000_000);

/// The amount of yocto NEAR the contract dedicates to guarantee that the "share" price never
/// decreases. It's used during rounding errors for share -> amount conversions.
pub const STAKE_SHARE_PRICE_GUARANTEE_FUND: Balance = 1_000_000_000_000;

/// There is no deposit balance attached.
const NO_DEPOSIT: Balance = 0;

/// Maximum number of active farms at one time.
const MAX_NUM_ACTIVE_FARMS: usize = 3;

/// The number of epochs required for the locked balance to become unlocked.
/// NOTE: The actual number of epochs when the funds are unlocked is 3. But there is a corner case
/// when the unstaking promise can arrive at the next epoch, while the inner state is already
/// updated in the previous epoch. It will not unlock the funds for 4 epochs.
const NUM_EPOCHS_TO_UNLOCK: EpochHeight = 4;

const OPTIMISTIC_NUM_EPOCHS_TO_UNLOCK: EpochHeight = 3;

construct_uint! {
    /// 256-bit unsigned integer.
    #[derive(BorshSerialize, BorshDeserialize)]
    pub struct U256(4);
}

#[derive(BorshStorageKey, BorshSerialize)]
pub enum StorageKeys {
    Accounts,
    Farms,
    AuthorizedUsers,
    AuthorizedFarmTokens,
    AccountRegistry,
    AccountsNotStakedStakingPool,
    OptimisticTimeExpectTokens,
}

/// Tracking balance for burning.
#[derive(BorshDeserialize, BorshSerialize)]
pub struct BurnInfo {
    /// The unstaked balance that can be burnt.
    pub unstaked: Balance,
    /// Number of "stake" shares that must be burnt.
    pub stake_shares: Balance,
    /// The minimum epoch height when the burn is allowed.
    pub unstaked_available_epoch_height: EpochHeight,
}

/// Updatable reward fee only after NUM_EPOCHS_TO_UNLOCK.
#[derive(BorshDeserialize, BorshSerialize)]
pub struct UpdatableRewardFee {
    reward_fee_fraction: Ratio,
    next_reward_fee_fraction: Ratio,
    available_epoch_height: EpochHeight,
}

impl UpdatableRewardFee {
    pub fn new(reward_fee_fraction: Ratio) -> Self {
        Self {
            reward_fee_fraction: reward_fee_fraction.clone(),
            next_reward_fee_fraction: reward_fee_fraction,
            available_epoch_height: 0,
        }
    }

    pub fn current(&self) -> &Ratio {
        if env::epoch_height() >= self.available_epoch_height {
            &self.next_reward_fee_fraction
        } else {
            &self.reward_fee_fraction
        }
    }

    pub fn next(&self) -> &Ratio {
        &self.next_reward_fee_fraction
    }

    pub fn set(&mut self, next_reward_fee_fraction: Ratio) {
        if env::epoch_height() >= self.available_epoch_height {
            self.reward_fee_fraction = self.next_reward_fee_fraction.clone();
        }
        self.next_reward_fee_fraction = next_reward_fee_fraction;
        self.available_epoch_height = env::epoch_height() + NUM_EPOCHS_TO_UNLOCK
    }
}

/// Inner account data of a delegate.
#[derive(BorshDeserialize, BorshSerialize, Debug, PartialEq)]
pub struct OldStateAccountWithReward{
    /// The unstaked balance. It represents the amount the account has on this contract that
    /// can either be staked or withdrawn.
    pub unstaked: Balance,
    /// The amount of "stake" shares. Every stake share corresponds to the amount of staked balance.
    /// NOTE: The number of shares should always be less or equal than the amount of staked balance.
    /// This means the price of stake share should always be at least `1`.
    /// The price of stake share can be computed as `total_staked_balance` / `total_stake_shares`.
    pub stake: Balance,
    /// The minimum epoch height when the withdrawn is allowed.
    /// This changes after unstaking action, because the amount is still locked for 3 epochs.
    pub unstaked_available_epoch_height: EpochHeight,
    /// The reward that has been paid to the account
    pub reward_tally: Balance,
    /// Bool variable showing whether the reward_tally is positive or negative
    pub tally_below_zero: bool,
    /// Last claimed reward for each active farm.
    pub last_farm_reward_per_share: HashMap<u64, U256>,
    /// Farmed tokens withdrawn from the farm but not from the contract.
    pub amounts: HashMap<AccountId, Balance>,
}

/// Structure containing information for accounts that have their rewards not being restaked
#[derive(BorshDeserialize, BorshSerialize)]
pub struct OldStateInnerStakingPoolWithoutRewardsRestaked{
    pub accounts: UnorderedMap<AccountId, OldStateAccountWithReward>,
    /// Pool total staked balance
    pub total_staked_balance: Balance,
    /// Accounts deposit, it would be used when calculating how much of the total rewards is for each account
    /// and also how much of the total staked balance can be unstaked
    pub reward_per_token: Fraction,
}

#[derive(BorshDeserialize, BorshSerialize)]
pub struct OldVersionStakingContract {
    /// The public key which is used for staking action. It's the public key of the validator node
    /// that validates on behalf of the pool.
    pub stake_public_key: PublicKey,
    /// The last epoch height when `ping` was called.
    pub last_epoch_height: EpochHeight,
    /// The last total balance of the account (consists of staked and unstaked balances).
    pub last_total_balance: Balance,
    /// The total amount to burn that will be available.
    /// The fraction of the reward that goes to the owner of the staking pool for running the
    /// validator node.
    pub reward_fee_fraction: UpdatableRewardFee,
    /// The fraction of the reward that gets burnt.
    pub burn_fee_fraction: Ratio,
    /// Farm tokens.
    pub farms: Vector<Farm>,
    /// Active farms: indicies into `farms`.
    pub active_farms: Vec<u64>,
    /// Whether the staking is paused.
    /// When paused, the account unstakes everything (stakes 0) and doesn't restake.
    /// It doesn't affect the staking shares or reward distribution.
    /// Pausing is useful for node maintenance. Only the owner can pause and resume staking.
    /// The contract is not paused by default.
    pub paused: bool,
    /// Authorized users, allowed to add farms.
    /// This is done to prevent farm spam with random tokens.
    /// Should not be a large number.
    pub authorized_users: UnorderedSet<AccountId>,
    /// Authorized tokens for farms.
    /// Required because any contract can call method with ft_transfer_call, so must verify that contract will accept it.
    pub authorized_farm_tokens: UnorderedSet<AccountId>,
    /// Inner staking pool, that restakes the received rewards
    pub rewards_staked_staking_pool: InnerStakingPool,
    /// Inner staking pool, that doesnt restake rewards
    pub rewards_not_staked_staking_pool: OldStateInnerStakingPoolWithoutRewardsRestaked,
    /// Map showing whether account has his rewards staked or unstaked
    pub account_pool_register: UnorderedMap<AccountId, bool>,
}

impl OldVersionStakingContract{
    pub fn new(
        owner_id: AccountId,
        stake_public_key: PublicKey,
        balance: Balance
    ) -> Self {
        //assert!(!env::state_exists(), "Already initialized");
        
        
        let account_balance = balance;
        let total_staked_balance = account_balance - STAKE_SHARE_PRICE_GUARANTEE_FUND;

        let mut this = Self {
            stake_public_key: stake_public_key.into(),
            last_epoch_height: env::epoch_height(),
            last_total_balance: account_balance,
            reward_fee_fraction: UpdatableRewardFee::new(Ratio { numerator: 1, denominator: 10 }),
            burn_fee_fraction: Ratio {numerator:1, denominator: 5},
            farms: Vector::new(b"oldstatef".to_vec()),
            active_farms: Vec::new(),
            paused: false,
            authorized_users: UnorderedSet::new(b"oldstateau".to_vec()),
            authorized_farm_tokens: UnorderedSet::new(b"oldstateaft".to_vec()),
            rewards_staked_staking_pool: InnerStakingPool::new(NumStakeShares::from(total_staked_balance), total_staked_balance, 0),
            rewards_not_staked_staking_pool: OldStateInnerStakingPoolWithoutRewardsRestaked {reward_per_token: Fraction::new (0,1), total_staked_balance: 0, accounts: UnorderedMap::new(b"oldacc".to_vec())},
            account_pool_register: UnorderedMap::new(b"oldstate_apr".to_vec()),
        };

        this
    }
}

#[near_bindgen]
#[derive(BorshDeserialize, BorshSerialize)]
pub struct StakingContract {
    /// The public key which is used for staking action. It's the public key of the validator node
    /// that validates on behalf of the pool.
    pub stake_public_key: PublicKey,
    /// The last epoch height when `ping` was called.
    pub last_epoch_height: EpochHeight,
    /// The last total balance of the account (consists of staked and unstaked balances).
    pub last_total_balance: Balance,
    pub last_balance_in_contract: Balance,
    /// the structure contains on which epoch, what amount of tokens are expected
    /// to arrive after unstaking a specific amount of tokens. They are received as part of the account balance
    /// because there is not an actual transaction for the unstaking. The structure will collect the optimistic
    /// arrival time of tokens, usualy 3 epochs, but because the promise could arrive at the next epoch so
    /// it could take 4 epochs.
    /// First part of tuple is for amount from unstaking, the second is for not staked rewards
    pub optimistic_expected_tokens: UnorderedMap<EpochHeight, ExpectedTokensInEpoch>,

    /// The total amount to burn that will be available.
    /// The fraction of the reward that goes to the owner of the staking pool for running the
    /// validator node.
    pub reward_fee_fraction: UpdatableRewardFee,
    /// The fraction of the reward that gets burnt.
    pub burn_fee_fraction: Ratio,
    /// Farm tokens.
    pub farms: Vector<Farm>,
    /// Active farms: indicies into `farms`.
    pub active_farms: Vec<u64>,
    /// Whether the staking is paused.
    /// When paused, the account unstakes everything (stakes 0) and doesn't restake.
    /// It doesn't affect the staking shares or reward distribution.
    /// Pausing is useful for node maintenance. Only the owner can pause and resume staking.
    /// The contract is not paused by default.
    pub paused: bool,
    /// Authorized users, allowed to add farms.
    /// This is done to prevent farm spam with random tokens.
    /// Should not be a large number.
    pub authorized_users: UnorderedSet<AccountId>,
    /// Authorized tokens for farms.
    /// Required because any contract can call method with ft_transfer_call, so must verify that contract will accept it.
    pub authorized_farm_tokens: UnorderedSet<AccountId>,
    /// Inner staking pool, that restakes the received rewards
    pub rewards_staked_staking_pool: InnerStakingPool,
    /// Inner staking pool, that doesnt restake rewards
    pub rewards_not_staked_staking_pool: InnerStakingPoolWithoutRewardsRestaked,
    /// Map showing whether account has his rewards staked or unstaked
    pub account_pool_register: UnorderedMap<AccountId, bool>,
}

impl Default for StakingContract {
    fn default() -> Self {
        panic!("Staking contract should be initialized before usage")
    }
}

#[derive(BorshDeserialize, BorshSerialize, Clone, PartialEq, Debug)]
pub struct ExpectedTokensInEpoch{
    pub unstaked_amount: Balance,
    pub not_staked_reward_amount: Balance,
}

impl Default for ExpectedTokensInEpoch{
    fn default() -> Self{
        Self { unstaked_amount: 0, not_staked_reward_amount: 0 }
    }
}

#[derive(BorshDeserialize, BorshSerialize, Serialize, Deserialize, Clone, PartialEq, Debug)]
#[serde(crate = "near_sdk::serde")]
pub struct Ratio {
    pub numerator: u32,
    pub denominator: u32,
}

impl Ratio {
    pub fn assert_valid(&self) {
        assert_ne!(self.denominator, 0, "Denominator must be a positive number");
        assert!(
            self.numerator <= self.denominator,
            "The reward fee must be less or equal to 1"
        );
    }

    pub fn multiply(&self, value: Balance) -> Balance {
        if self.denominator == 0 || self.numerator == 0 {
            0
        } else {
            (U256::from(self.numerator) * U256::from(value) / U256::from(self.denominator))
                .as_u128()
        }
    }
}

#[derive(BorshDeserialize, BorshSerialize)]
pub struct OldStakingContract {
    /// The public key which is used for staking action. It's the public key of the validator node
    /// that validates on behalf of the pool.
    pub stake_public_key: PublicKey,
    /// The last epoch height when `ping` was called.
    pub last_epoch_height: EpochHeight,
    /// The last total balance of the account (consists of staked and unstaked balances).
    pub last_total_balance: Balance,
    /// The total amount of shares. It should be equal to the total amount of shares across all
    /// accounts.
    pub total_stake_shares: NumStakeShares,
    /// The total staked balance.
    pub total_staked_balance: Balance,
    /// The total burn share balance, that will not be accounted in the farming.
    pub total_burn_shares: NumStakeShares,
    /// The total amount to burn that will be available.
    /// The fraction of the reward that goes to the owner of the staking pool for running the
    /// validator node.
    pub reward_fee_fraction: UpdatableRewardFee,
    /// The fraction of the reward that gets burnt.
    pub burn_fee_fraction: Ratio,
    /// Persistent map from an account ID to the corresponding account.
    pub accounts: UnorderedMap<AccountId, Account>,
    /// Farm tokens.
    pub farms: Vector<Farm>,
    /// Active farms: indicies into `farms`.
    pub active_farms: Vec<u64>,
    /// Whether the staking is paused.
    /// When paused, the account unstakes everything (stakes 0) and doesn't restake.
    /// It doesn't affect the staking shares or reward distribution.
    /// Pausing is useful for node maintenance. Only the owner can pause and resume staking.
    /// The contract is not paused by default.
    pub paused: bool,
    /// Authorized users, allowed to add farms.
    /// This is done to prevent farm spam with random tokens.
    /// Should not be a large number.
    pub authorized_users: UnorderedSet<AccountId>,
    /// Authorized tokens for farms.
    /// Required because any contract can call method with ft_transfer_call, so must verify that contract will accept it.
    pub authorized_farm_tokens: UnorderedSet<AccountId>,
}

#[near_bindgen]
impl StakingContract {

    //#[private]
    #[init(ignore_state)]
    pub fn migrate_state_from_previous_version() -> Self{
        log!("0");
        let old_state: OldVersionStakingContract = env::state_read().expect("failed");
        log!("1");
        let mut this = Self{
            stake_public_key: old_state.stake_public_key,
            last_epoch_height: old_state.last_epoch_height,
            last_balance_in_contract: env::account_balance(),
            optimistic_expected_tokens: UnorderedMap::new(StorageKeys::OptimisticTimeExpectTokens),
            last_total_balance: old_state.last_total_balance,
            reward_fee_fraction: old_state.reward_fee_fraction,
            burn_fee_fraction: old_state.burn_fee_fraction,
            farms: old_state.farms,
            active_farms: old_state.active_farms,
            paused: old_state.paused,
            authorized_users: old_state.authorized_users,
            authorized_farm_tokens: old_state.authorized_farm_tokens,
            rewards_staked_staking_pool: old_state.rewards_staked_staking_pool,
            rewards_not_staked_staking_pool: InnerStakingPoolWithoutRewardsRestaked::new(),
            account_pool_register: old_state.account_pool_register,
        };
        log!("2");
        this.rewards_not_staked_staking_pool.reward_per_token = Fraction::new(old_state.rewards_not_staked_staking_pool.reward_per_token.numerator, old_state.rewards_not_staked_staking_pool.reward_per_token.denominator);
        this.rewards_not_staked_staking_pool.total_buffered_rewards = 0;
        this.rewards_not_staked_staking_pool.total_staked_balance = old_state.rewards_not_staked_staking_pool.total_staked_balance;
        this.rewards_not_staked_staking_pool.total_rewards = this.rewards_not_staked_staking_pool.reward_per_token.multiply(this.rewards_not_staked_staking_pool.total_staked_balance);
        
        let old_state_acc_vec_iter = old_state.rewards_not_staked_staking_pool.accounts.iter();
        log!("3");
        for element in old_state_acc_vec_iter{
            log!("{}", element.0);
            let acc: AccountWithReward = AccountWithReward { 
                unstaked: element.1.unstaked, 
                stake: element.1.stake, 
                unstaked_available_epoch_height: element.1.unstaked_available_epoch_height, 
                reward_tally: element.1.reward_tally, 
                tally_below_zero: element.1.tally_below_zero, 
                payed_reward: 0, 
                last_farm_reward_per_share: element.1.last_farm_reward_per_share.clone(), 
                amounts: element.1.amounts.clone() 
            };

            //log!("4 {} {}", element.0, acc.);
            this.rewards_not_staked_staking_pool.accounts.insert(&element.0, &acc);
        }

        log!("5");
        return this;
    }

    #[private]
    #[init(ignore_state)]
    pub fn migrate_state() -> Self{
        let old_state: OldStakingContract = env::state_read().expect("failed");

        let mut this = Self{
            stake_public_key: old_state.stake_public_key,
            last_epoch_height: old_state.last_epoch_height,
            last_balance_in_contract: env::account_balance(),
            optimistic_expected_tokens: UnorderedMap::new(StorageKeys::OptimisticTimeExpectTokens),
            last_total_balance: old_state.last_total_balance,
            reward_fee_fraction: old_state.reward_fee_fraction,
            burn_fee_fraction: old_state.burn_fee_fraction,
            farms: old_state.farms,
            active_farms: old_state.active_farms,
            paused: old_state.paused,
            authorized_users: old_state.authorized_users,
            authorized_farm_tokens: old_state.authorized_farm_tokens,
            rewards_staked_staking_pool: InnerStakingPool::new(NumStakeShares::from(old_state.total_staked_balance), old_state.total_staked_balance, old_state.total_burn_shares),
            rewards_not_staked_staking_pool: InnerStakingPoolWithoutRewardsRestaked::new(),
            account_pool_register: UnorderedMap::new(StorageKeys::AccountRegistry),
        };

        this.rewards_staked_staking_pool.accounts = old_state.accounts;
        let owner_id = Self::internal_get_owner_id();

        this.internal_register_account_to_staking_pool(&owner_id, true);
        this.internal_register_account_to_staking_pool(&AccountId::new_unchecked(ZERO_ADDRESS.to_string()), true);

        return this;
    }

    /// Initializes the contract with the given owner_id, initial staking public key (with ED25519
    /// curve) and initial reward fee fraction that owner charges for the validation work.
    ///
    /// The entire current balance of this contract will be used to stake. This allows contract to
    /// always maintain staking shares that can't be unstaked or withdrawn.
    /// It prevents inflating the price of the share too much.
    #[init]
    pub fn new(
        owner_id: AccountId,
        stake_public_key: PublicKey,
        reward_fee_fraction: Ratio,
        burn_fee_fraction: Ratio,
    ) -> Self {
        assert!(!env::state_exists(), "Already initialized");
        reward_fee_fraction.assert_valid();
        assert!(
            env::is_valid_account_id(owner_id.as_bytes()),
            "The owner account ID is invalid"
        );
        let account_balance = env::account_balance();
        let total_staked_balance = account_balance - STAKE_SHARE_PRICE_GUARANTEE_FUND;
        assert_eq!(
            env::account_locked_balance(),
            0,
            "The staking pool shouldn't be staking at the initialization"
        );
        let mut this = Self {
            stake_public_key: stake_public_key.into(),
            last_epoch_height: env::epoch_height(),
            last_total_balance: account_balance,
            last_balance_in_contract: account_balance - total_staked_balance,
            optimistic_expected_tokens: UnorderedMap::new(StorageKeys::OptimisticTimeExpectTokens),
            reward_fee_fraction: UpdatableRewardFee::new(reward_fee_fraction),
            burn_fee_fraction,
            farms: Vector::new(StorageKeys::Farms),
            active_farms: Vec::new(),
            paused: false,
            authorized_users: UnorderedSet::new(StorageKeys::AuthorizedUsers),
            authorized_farm_tokens: UnorderedSet::new(StorageKeys::AuthorizedFarmTokens),
            rewards_staked_staking_pool: InnerStakingPool::new(NumStakeShares::from(total_staked_balance), total_staked_balance, 0),
            rewards_not_staked_staking_pool: InnerStakingPoolWithoutRewardsRestaked::new(),
            account_pool_register: UnorderedMap::new(StorageKeys::AccountRegistry),
        };
        Self::internal_set_owner(&owner_id);
        Self::internal_set_factory(&env::predecessor_account_id());
        Self::internal_set_version();
        this.internal_register_account_to_staking_pool(&owner_id, true);
        this.internal_register_account_to_staking_pool(&AccountId::new_unchecked(ZERO_ADDRESS.to_string()), true);
        // Staking with the current pool to make sure the staking key is valid.
        this.internal_restake();
        this
    }

    /// Distributes rewards and restakes if needed.
    pub fn ping(&mut self) {
        if self.internal_ping() {
            self.internal_restake();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use near_contract_standards::fungible_token::receiver::FungibleTokenReceiver;
    use near_sdk::json_types::U64;
    use near_sdk::mock::VmAction;
    use near_sdk::serde_json::json;
    use near_sdk::test_utils::{get_created_receipts, testing_env_with_promise_results};

    use crate::staking_pool::Fraction;
    use crate::test_utils::tests::*;
    use crate::test_utils::*;

    use super::*;

    #[test]
    fn test_restake_fail() {
        let pub_key = "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
            .parse()
            .unwrap();
        let mut emulator = Emulator::new(owner(), pub_key, zero_fee(), Option::None,);
        emulator.update_context(bob(), 0);
        emulator.contract.internal_restake();
        let receipts = get_created_receipts();
        assert_eq!(receipts.len(), 2);
        // Mocked Receipt fields are private, so can't check directly.
        if let VmAction::Stake { stake, .. } = receipts[0].actions[0] {
            assert_eq!(stake, 29999999999999000000000000);
        } else {
            panic!("unexpected action");
        }
        if let VmAction::FunctionCall { method_name, .. } = &receipts[1].actions[0] {
            assert_eq!(method_name.as_bytes(), b"on_stake_action")
        } else {
            panic!("unexpected action");
        }

        emulator.simulate_stake_call();

        emulator.update_context(staking(), 0);
        testing_env_with_promise_results(emulator.context.clone(), PromiseResult::Failed);
        emulator.contract.on_stake_action();
        let receipts = get_created_receipts();
        assert_eq!(receipts.len(), 1);
        if let VmAction::Stake { stake, .. } = receipts[0].actions[0] {
            assert_eq!(stake, 0);
        } else {
            panic!("unexpected action");
        }
    }

    #[test]
    fn test_deposit_withdraw() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        let deposit_amount = ntoy(1_000_000);
        emulator.update_context(bob(), deposit_amount);
        emulator.contract.deposit();
        emulator.amount += deposit_amount;
        emulator.update_context(bob(), 0);
        assert_eq!(
            emulator.contract.get_account_unstaked_balance(bob()).0,
            deposit_amount
        );
        emulator.contract.withdraw(deposit_amount.into());
        assert_eq!(
            emulator.contract.get_account_unstaked_balance(bob()).0,
            0u128
        );
    }

    #[test]
    fn test_stake_with_fee() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            Ratio {
                numerator: 10,
                denominator: 100,
            },
            Option::None,
        );
        let deposit_amount = ntoy(1_000_000);
        emulator.update_context(bob(), deposit_amount);
        emulator.contract.deposit();
        emulator.amount += deposit_amount;
        emulator.update_context(bob(), 0);
        emulator.contract.stake(deposit_amount.into());
        emulator.simulate_stake_call();
        assert_eq!(
            emulator.contract.get_account_staked_balance(bob()).0,
            deposit_amount
        );

        let locked_amount = emulator.locked_amount;
        let n_locked_amount = yton(locked_amount);
        emulator.skip_epochs(10);
        // Overriding rewards (+ 100K reward)
        emulator.locked_amount = locked_amount + ntoy(100_000);
        emulator.update_context(bob(), 0);
        emulator.contract.ping();
        let expected_amount = deposit_amount
            + ntoy((yton(deposit_amount) * 90_000 + n_locked_amount / 2) / n_locked_amount);
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(bob()).0,
            expected_amount
        );
        // Owner got 10% of the rewards
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(owner()).0,
            ntoy(10_000)
        );

        let locked_amount = emulator.locked_amount;
        let n_locked_amount = yton(locked_amount);
        emulator.skip_epochs(10);
        // Overriding rewards (another 100K reward)
        emulator.locked_amount = locked_amount + ntoy(100_000);

        emulator.update_context(bob(), 0);
        emulator.contract.ping();
        // previous balance plus (1_090_000 / 1_100_030)% of the 90_000 reward (rounding to nearest).
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(bob()).0,
            expected_amount
                + ntoy((yton(expected_amount) * 90_000 + n_locked_amount / 2) / n_locked_amount)
        );
        // owner earns 10% with the fee and also small percentage from restaking.
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(owner()).0,
            ntoy(10_000)
                + ntoy(10_000)
                + ntoy((10_000u128 * 90_000 + n_locked_amount / 2) / n_locked_amount)
        );

        assert_eq!(emulator.contract.get_number_of_accounts(), 2);
    }

    #[test]
    fn test_stake_unstake() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        let deposit_amount = ntoy(1_000_000);
        emulator.update_context(bob(), deposit_amount);
        emulator.contract.deposit();
        emulator.amount += deposit_amount;
        emulator.update_context(bob(), 0);
        emulator.contract.stake(deposit_amount.into());
        emulator.simulate_stake_call();
        assert_eq!(
            emulator.contract.get_account_staked_balance(bob()).0,
            deposit_amount
        );
        let locked_amount = emulator.locked_amount;
        // 10 epochs later, unstake half of the money.
        emulator.skip_epochs(10);
        // Overriding rewards
        emulator.locked_amount = locked_amount + ntoy(10);
        emulator.update_context(bob(), 0);
        emulator.contract.ping();
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(bob()).0,
            deposit_amount + ntoy(10)
        );
        emulator.contract.unstake((deposit_amount / 2).into());
        emulator.simulate_stake_call();
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(bob()).0,
            deposit_amount / 2 + ntoy(10)
        );
        assert_eq_in_near!(
            emulator.contract.get_account_unstaked_balance(bob()).0,
            deposit_amount / 2
        );
        let acc = emulator.contract.get_account(bob());
        assert_eq!(acc.account_id, bob());
        assert_eq_in_near!(acc.unstaked_balance.0, deposit_amount / 2);
        assert_eq_in_near!(acc.staked_balance.0, deposit_amount / 2 + ntoy(10));
        assert!(!acc.can_withdraw);

        assert!(!emulator
            .contract
            .is_account_unstaked_balance_available(bob()),);
        emulator.skip_epochs(4);
        emulator.update_context(bob(), 0);
        assert!(emulator
            .contract
            .is_account_unstaked_balance_available(bob()),);
    }

    #[test]
    fn test_stake_all_unstake_all() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        let deposit_amount = ntoy(1_000_000);
        emulator.update_context(bob(), deposit_amount);
        emulator.contract.deposit_and_stake();
        emulator.amount += deposit_amount;
        emulator.simulate_stake_call();
        assert_eq!(
            emulator.contract.get_account_staked_balance(bob()).0,
            deposit_amount
        );
        assert_eq_in_near!(emulator.contract.get_account_unstaked_balance(bob()).0, 0);
        let locked_amount = emulator.locked_amount;

        // 10 epochs later, unstake all.
        emulator.skip_epochs(10);
        // Overriding rewards
        emulator.locked_amount = locked_amount + ntoy(10);
        emulator.update_context(bob(), 0);
        emulator.contract.ping();
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(bob()).0,
            deposit_amount + ntoy(10)
        );
        emulator.contract.unstake_all();
        emulator.simulate_stake_call();
        assert_eq_in_near!(emulator.contract.get_account_staked_balance(bob()).0, 0);
        assert_eq_in_near!(
            emulator.contract.get_account_unstaked_balance(bob()).0,
            deposit_amount + ntoy(10)
        );
    }

    /// Test that two can delegate and then undelegate their funds and rewards at different time.
    #[test]
    fn test_two_delegates() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        emulator.update_context(alice(), ntoy(1_000_000));
        emulator.contract.deposit();
        emulator.amount += ntoy(1_000_000);
        emulator.update_context(alice(), 0);
        emulator.contract.stake(ntoy(1_000_000).into());
        emulator.simulate_stake_call();
        emulator.skip_epochs(3);
        emulator.update_context(bob(), ntoy(1_000_000));

        emulator.contract.deposit();
        emulator.amount += ntoy(1_000_000);
        emulator.update_context(bob(), 0);
        emulator.contract.stake(ntoy(1_000_000).into());
        emulator.simulate_stake_call();
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(bob()).0,
            ntoy(1_000_000)
        );
        emulator.skip_epochs(3);
        emulator.update_context(alice(), 0);
        emulator.contract.ping();
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(alice()).0,
            ntoy(1_060_900) - 1
        );
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(bob()).0,
            ntoy(1_030_000)
        );

        // Checking accounts view methods
        // Should be 2, because the pool has 0 fee.
        assert_eq!(emulator.contract.get_number_of_accounts(), 2);
        let accounts = emulator.contract.get_accounts(0, 10);
        assert_eq!(accounts.len(), 2);
        assert_eq!(accounts[0].account_id, alice());
        assert_eq!(accounts[1].account_id, bob());

        let accounts = emulator.contract.get_accounts(1, 10);
        assert_eq!(accounts.len(), 1);
        assert_eq!(accounts[0].account_id, bob());

        let accounts = emulator.contract.get_accounts(0, 1);
        assert_eq!(accounts.len(), 1);
        assert_eq!(accounts[0].account_id, alice());

        let accounts = emulator.contract.get_accounts(2, 10);
        assert_eq!(accounts.len(), 0);
    }

    #[test]
    fn test_low_balances() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        let initial_balance = 100;
        emulator.update_context(alice(), initial_balance);
        emulator.contract.deposit();
        emulator.amount += initial_balance;
        let mut remaining = initial_balance;
        let mut amount = 1;
        while remaining >= 4 {
            emulator.update_context(alice(), 0);
            amount = 2 + (amount - 1) % 3;
            emulator.contract.stake(amount.into());
            emulator.simulate_stake_call();
            remaining -= amount;
        }
    }

    #[test]
    fn test_rewards() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        let initial_balance = ntoy(100);
        emulator.update_context(alice(), initial_balance);
        emulator.contract.deposit();
        emulator.amount += initial_balance;
        let mut remaining = 100;
        let mut amount = 1;
        while remaining >= 4 {
            emulator.skip_epochs(3);
            emulator.update_context(alice(), 0);
            emulator.contract.ping();
            emulator.update_context(alice(), 0);
            amount = 2 + (amount - 1) % 3;
            emulator.contract.stake(ntoy(amount).into());
            emulator.simulate_stake_call();
            remaining -= amount;
        }
    }

    #[test]
    fn test_farm() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        emulator.update_context(owner(), 0);
        emulator.contract.add_authorized_farm_token(&bob());
        add_farm(&mut emulator, ntoy(100));

        emulator.deposit_and_stake(alice(), ntoy(1_000_000));

        let farm = emulator.contract.get_farm(0);
        assert_eq!(farm.name, "test".to_string());
        assert_eq!(farm.token_id, bob());
        assert_eq!(farm.start_date.0, 0);
        assert_eq!(farm.end_date.0, ONE_EPOCH_TS * 4);
        assert_eq!(farm.amount.0, ntoy(100));

        assert_eq!(emulator.contract.get_unclaimed_reward(alice(), 0).0, 0);

        emulator.skip_epochs(1);
        emulator.update_context(alice(), 0);

        // First user got 1/4 of the rewards after 1/4 of the time.
        assert!(almost_equal(
            emulator.contract.get_unclaimed_reward(alice(), 0).0,
            ntoy(25),
            ntoy(1) / 100
        ));

        // Adding second user.
        emulator.deposit_and_stake(charlie(), ntoy(1_000_000));

        emulator.skip_epochs(1);
        assert!(almost_equal(
            emulator.contract.get_unclaimed_reward(alice(), 0).0,
            ntoy(375612) / 10000,
            ntoy(1) / 100
        ));
        let charlie_farmed = emulator.contract.get_unclaimed_reward(charlie(), 0).0;
        assert!(almost_equal(
            charlie_farmed,
            ntoy(124388) / 10000,
            ntoy(1) / 100
        ));

        emulator.deposit_and_stake(charlie(), ntoy(1_000_000));

        // Amount is still the same after depositing more without incrementing time.
        assert_eq!(
            emulator.contract.get_unclaimed_reward(charlie(), 0).0,
            charlie_farmed
        );

        emulator.skip_epochs(1);
        assert!(almost_equal(
            emulator.contract.get_unclaimed_reward(charlie(), 0).0,
            charlie_farmed + ntoy(165834) / 10000,
            ntoy(1) / 100,
        ));

        emulator.update_context(alice(), 1);
        emulator.contract.claim(bob(), None);
        assert_eq!(emulator.contract.get_unclaimed_reward(alice(), 0).0, 0);
    }

    fn add_farm(emulator: &mut Emulator, amount: Balance) {
        emulator.update_context(bob(), 0);
        emulator.contract.ft_on_transfer(
            owner(),
            U128(amount),
            json!({
                "name": "test".to_string(),
                "start_date": U64(0),
                "end_date": U64(ONE_EPOCH_TS * 4),
            })
            .to_string(),
        );
    }

    #[test]
    fn test_stop_farm() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        emulator.update_context(owner(), 0);
        emulator.contract.add_authorized_farm_token(&bob());
        add_farm(&mut emulator, ntoy(100));
        emulator.deposit_and_stake(alice(), ntoy(1_000_000));
        emulator.skip_epochs(1);
        assert!(almost_equal(
            emulator.contract.get_unclaimed_reward(alice(), 0).0,
            ntoy(25),
            ntoy(1) / 100
        ));
        emulator.update_context(owner(), 0);
        emulator.contract.stop_farm(0);
        emulator.skip_epochs(1);
        // Deposit alice, start farm, wait for 1 epoch.
        // Stop farm, wait for another epoch - the amount of farmed tokens is the same.
        assert!(almost_equal(
            emulator.contract.get_unclaimed_reward(alice(), 0).0,
            ntoy(25),
            ntoy(1) / 100
        ));
    }

    #[test]
    #[should_panic(expected = "ERR_NOT_AUTHORIZED_TOKEN")]
    fn test_farm_not_authorized_token() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        add_farm(&mut emulator, ntoy(100));
    }

    #[test]
    #[should_panic(expected = "ERR_FARM_AMOUNT_TOO_SMALL")]
    fn test_farm_too_small_amount() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        emulator.update_context(owner(), 0);
        emulator.contract.add_authorized_farm_token(&bob());
        add_farm(&mut emulator, 100);
    }

    #[test]
    fn test_change_reward_fee() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Option::None,
        );
        assert_eq!(emulator.contract.get_reward_fee_fraction(), zero_fee());
        emulator.update_context(owner(), 0);
        let new_fee = Ratio {
            numerator: 1,
            denominator: 10,
        };
        emulator
            .contract
            .update_reward_fee_fraction(new_fee.clone());
        // The fee is still the old one.
        assert_eq!(emulator.contract.get_reward_fee_fraction(), zero_fee());
        assert_eq!(
            emulator
                .contract
                .get_pool_summary()
                .next_reward_fee_fraction,
            new_fee
        );
        emulator.skip_epochs(1);
        assert_eq!(emulator.contract.get_reward_fee_fraction(), zero_fee());
        emulator.skip_epochs(3);
        assert_eq!(emulator.contract.get_reward_fee_fraction(), new_fee);
        // Update once again.
        let new_fee2 = Ratio {
            numerator: 2,
            denominator: 10,
        };
        emulator.update_context(owner(), 0);
        emulator
            .contract
            .update_reward_fee_fraction(new_fee2.clone());
        assert_eq!(emulator.contract.get_reward_fee_fraction(), new_fee);
    }

    #[test]
    fn test_fraction(){
        let f = Fraction::new(17, 32);

        assert_eq!(f.multiply(43), 22);
        assert_eq_in_near!(ntoy(50), Fraction::new(1, 2).multiply(ntoy(100)));
        assert_eq!(*Fraction::new(3, 5).add(Fraction::new(3, 11)), Fraction::new(48, 55));

        let mut f1 = Fraction::new(1, 3);
        let f2 = Fraction::new(3, 5);
        f1.add(f2);
        assert_eq!(f1, Fraction::new(14,15));
        assert_eq_in_near!(f1.multiply(ntoy(30)), ntoy(28));

        assert_eq!(0, Fraction::new(1, 5).add(Fraction::new(1,5)).multiply(0));
        assert_eq!(Fraction::new(0, 1).add(Fraction::new(1,5)), &Fraction::new(1,5));
        assert_eq!(Fraction::new(1,5).add(Fraction::default()), &Fraction::new(1,5));
        assert_eq!(Fraction::new(0,1).add(Fraction::default()), &Fraction::default());

        let total_staked_balance: Balance = 32601004691073296566595668410 
            - (212391910925662939071534681 + 49558112549321352450024752);

        let mut rpt = Fraction::new (204574360324123112677640207, 32190000000000000000000000000);

        rpt.add(Fraction ::new (253413999866417394042330927, total_staked_balance));

    }

    #[test]
    fn test_two_staking_pools_total_balance(){
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7".parse().unwrap(),
            zero_fee(),
            Some(ntoy(100) + STAKE_SHARE_PRICE_GUARANTEE_FUND),
        );

        let bob_deposit_amount = ntoy(50);
        emulator.deposit_and_stake(bob(), bob_deposit_amount);
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(bob()).0,
            bob_deposit_amount
        );
        assert_eq_in_near!(
            emulator.contract.get_total_staked_balance().0,
            emulator.contract.rewards_staked_staking_pool.total_staked_balance
        );
        
        let alice_deposit = ntoy(100);
        let alice_stake = alice_deposit * 5 / 10;
        emulator.update_context(alice(), alice_deposit);
        emulator.contract.deposit_rewards_not_stake();
        emulator.amount += alice_deposit;
        emulator.contract.stake(alice_stake.into());
        emulator.simulate_stake_call();

        assert_eq_in_near!(
            emulator.contract.get_total_staked_balance().0,
            emulator.contract.rewards_staked_staking_pool.total_staked_balance +
            emulator.contract.rewards_not_staked_staking_pool.total_staked_balance
        );
        assert_eq_in_near!(
            emulator.contract.get_account_staked_balance(alice()).0,
            emulator.contract.rewards_not_staked_staking_pool.total_staked_balance
        );
        assert_eq_in_near!(emulator.contract.get_account_unstaked_balance(bob()).0, 0);
        let locked_amount = emulator.locked_amount;

        // 10 epochs later, unstake all.
        emulator.skip_epochs(10);
        let rewards = ntoy(10u128);
        // Overriding rewards
        emulator.locked_amount = locked_amount + rewards;
        // Compare total staked balance, before reward and after it
        // it should be increased by rewards variable without the rewards of the accounts
        // that are in the pool which doesnt restakes them, in this situation there is only 1 such account
        let mut total_staked_balance = emulator.contract.get_total_staked_balance().0;
        emulator.update_context(bob(), 0);
        emulator.contract.ping();
        /// the total staked balance in the contract is the 3/4 rewards for
        /// the staking pool that stakes rewards
        assert_eq_in_near!(emulator.contract.get_total_staked_balance().0, 
        total_staked_balance + rewards/4*3  - emulator.contract.get_account_not_staked_rewards(alice()).0);
        
        total_staked_balance = emulator.contract.get_total_staked_balance().0;
        emulator.update_context(alice(), 0);
        emulator.contract.unstake_all();

        assert_eq_in_near!(emulator.contract.get_total_staked_balance().0, total_staked_balance - alice_stake);
    }

    #[test]
    fn test_check_rewards(){
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7".parse().unwrap(),
            zero_fee(),
            Some(ntoy(100) + STAKE_SHARE_PRICE_GUARANTEE_FUND),
        );

        let bob_deposit_amount = ntoy(100);
        emulator.update_context(bob(), bob_deposit_amount);
        emulator.contract.deposit_rewards_not_stake();
        emulator.amount+=bob_deposit_amount;
        emulator.contract.stake(bob_deposit_amount.into());
        emulator.simulate_stake_call();

        let locked_amount = emulator.locked_amount;

        // 10 epochs later, unstake all.
        emulator.skip_epochs(10);

        let rewards = ntoy(10u128);
        // Overriding rewards
        emulator.locked_amount = locked_amount + rewards;
        emulator.update_context(alice(), 0);
        emulator.contract.ping();
        emulator.update_context(bob(), 0);
        println!("{}", yton(rewards*5/10));
        emulator.skip_epochs_and_set_reward(3, 0);
        emulator.transfer_from_locked_amount_to_contract_balance(ntoy(5));
        emulator.contract.ping();

        assert_eq_in_near!(emulator.contract.get_account_not_staked_rewards(bob()).0, rewards * 5 / 10);
        assert_eq_in_near!(emulator.contract.get_account_staked_balance(bob()).0, bob_deposit_amount);
        assert_eq_in_near!(emulator.contract.get_account_unstaked_balance(bob()).0, 0);
        
        emulator.update_context(bob(), 0);

        emulator.contract.unstake_all();
        emulator.simulate_stake_call();
        assert_eq_in_near!(emulator.contract.get_account_staked_balance(bob()).0, 0);
        assert_eq_in_near!(emulator.contract.get_account_unstaked_balance(bob()).0, bob_deposit_amount);
        emulator.skip_epochs_and_set_reward(3, 0);
        emulator.contract.ping();
        emulator.skip_epochs_and_set_reward(1, 0);

        emulator.update_context(bob(), 0);
        println!("Bob rewards {}, rewards in near {}", emulator.contract.get_account_not_staked_rewards(bob()).0,
    yton(emulator.contract.get_account_not_staked_rewards(bob()). 0));
        assert_eq_in_near!(emulator.contract.get_account_not_staked_rewards(bob()).0, rewards * 5 / 10);
        emulator.contract.withdraw_all();
        assert_eq!(emulator.contract.get_account_staked_balance(bob()).0, 0);
        assert_eq!(emulator.contract.get_account_unstaked_balance(bob()).0, 0);
        assert_eq!(emulator.contract.get_account_not_staked_rewards(bob()).0, 0);
    }

    #[test]
    fn test_stake_two_pools(){
        let initial_balance = ntoy(100) + STAKE_SHARE_PRICE_GUARANTEE_FUND;
        let initial_stake = initial_balance - STAKE_SHARE_PRICE_GUARANTEE_FUND;
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7".parse().unwrap(),
            zero_fee(),
            Some(initial_balance),
        );

        let mut future_rewards = UnorderedMap::new("fr".as_bytes());

        // 5 delegators A, B, C, D, E
        // A, B -> first pool that stakes rewards (50, 100 (stakes 50))
        // C, E -> second pool that saves the rewards (60, 40)
        // D -> comes after first iteration of rewards is distributes in the second pool (100)
        
        let a_deposit_amount = ntoy(50);
        let b_deposit_amount = ntoy(100);
        let c_deposit_amount = ntoy(60);
        let e_deposit_amount = ntoy(40);
        let d_deposit_amount = ntoy(100);
        // A deposits and stakes all
        emulator.deposit_and_stake(a(), a_deposit_amount);

        // B deposits and stakes 50/100
        emulator.update_context(b(), b_deposit_amount);
        emulator.contract.deposit();
        emulator.amount += b_deposit_amount;
        let b_stake_amount = b_deposit_amount * 5 / 10;
        emulator.contract.stake((b_stake_amount).into());
        emulator.simulate_stake_call();

        // C deposits and stakes all
        emulator.update_context(c(), c_deposit_amount);
        emulator.contract.deposit_and_stake_rewards_not_stake();
        emulator.amount += c_deposit_amount;
        emulator.simulate_stake_call();

        // E deposits and stakes all
        emulator.update_context(e(), e_deposit_amount);
        emulator.contract.deposit_rewards_not_stake();
        emulator.amount += e_deposit_amount;
        emulator.contract.stake_all();
        emulator.simulate_stake_call();

        let mut rewards = ntoy(60);
        // override rewards
        emulator.skip_epochs_and_set_reward(1, rewards);
        emulator.distribute_rewards_between_pools(&mut future_rewards, rewards, 3);
        emulator.pay_rewards_on_epoch(&future_rewards);

        emulator.update_context(a(), 0);
        let mut expected_reward = emulator.locked_amount + emulator.amount - emulator.contract.last_total_balance;
        assert_eq!(rewards, expected_reward);
        emulator.contract.ping();
        emulator.update_context(a(), 0);
        assert_eq_in_near!(emulator.contract.rewards_staked_staking_pool.total_staked_balance, 
            a_deposit_amount + b_stake_amount + ntoy(40) + initial_stake);

        assert_eq_in_near!(emulator.contract.rewards_not_staked_staking_pool.total_staked_balance,
            c_deposit_amount + e_deposit_amount);

        /// stil the same epoch rewards are not being transfered from locked account balance to account balance
        assert_eq_in_near!(emulator.contract.get_account_not_staked_rewards(c()).0, 0);
        //emulator.simulate_stake_call();

        // D deposit and stake
        emulator.update_context(d(), d_deposit_amount);
        emulator.contract.deposit_rewards_not_stake();
        emulator.amount += d_deposit_amount;
        emulator.contract.stake_all();
        emulator.simulate_stake_call();

        rewards = ntoy(44);
        emulator.skip_epochs_and_set_reward(1, rewards);
        emulator.distribute_rewards_between_pools(&mut future_rewards, rewards, 3);
        emulator.pay_rewards_on_epoch(&future_rewards);

        emulator.update_context(a(), 0);
        expected_reward = emulator.locked_amount + emulator.amount - emulator.contract.last_total_balance;
        assert_eq!(rewards, expected_reward);
        emulator.contract.ping();

        println!("D rewards {}", emulator.contract.get_account_not_staked_rewards(d()).0);

        rewards = ntoy(116);
        emulator.skip_epochs_and_set_reward(1, rewards);
        emulator.distribute_rewards_between_pools(&mut future_rewards, rewards, 3);
        emulator.pay_rewards_on_epoch(&future_rewards);
        emulator.contract.ping();

        println!("D rewards {}", emulator.contract.get_account_not_staked_rewards(d()).0);

        rewards = ntoy(53);
        emulator.skip_epochs_and_set_reward(1, rewards);
        emulator.distribute_rewards_between_pools(&mut future_rewards, rewards, 3);
        emulator.pay_rewards_on_epoch(&future_rewards);
        emulator.contract.ping();

        println!("D rewards {}", emulator.contract.get_account_not_staked_rewards(d()).0);

        assert_eq_in_near!(emulator.contract.get_account_not_staked_rewards(d()).0, ntoy(8));
        assert_eq_in_near!(emulator.contract.get_account_not_staked_rewards(e()).0, ntoy(5));
        println!("Total staked balance for first pool {}", emulator.contract.rewards_staked_staking_pool.total_staked_balance);

        emulator.update_context(e(), 0);
        let e_rewards = emulator.contract.get_account_not_staked_rewards(e()).0;
        emulator.contract.withdraw_rewards(bob());
        emulator.amount -= e_rewards;

        assert_eq!(emulator.contract.get_account_not_staked_rewards(e()).0, 0);
        assert_eq_in_near!(emulator.contract.get_account_not_staked_rewards(d()).0, ntoy(8));

        emulator.update_context(b(), 0);
        assert_eq_in_near!(emulator.contract.get_account_staked_balance(b()).0, b_stake_amount + ntoy(41));
        println!("Total staked balance before B exit {}", yton(emulator.contract.rewards_staked_staking_pool.total_staked_balance));
        let b_staked = emulator.contract.get_account_staked_balance(b()).0;
        emulator.contract.unstake_all();
        
        rewards = ntoy(20);
        for i in 0..4{
            assert_eq!(emulator.contract.get_account(b()).can_withdraw, false);
            emulator.skip_epochs_and_set_reward(1, rewards);
            emulator.distribute_rewards_between_pools(&mut future_rewards, rewards, 3);
            emulator.pay_rewards_on_epoch(&future_rewards);
            emulator.contract.ping();  

            println!("D rewards {}", emulator.contract.get_account_not_staked_rewards(d()).0);
        }
        
        emulator.transfer_from_locked_amount_to_contract_balance(b_staked);
        assert_eq!(emulator.contract.get_account(b()).can_withdraw, true);
        assert_eq_in_near!(emulator.contract.get_account_unstaked_balance(b()).0, b_deposit_amount + ntoy(41));
        assert_eq!(emulator.contract.get_account_staked_balance(b()).0, 0);
        println!("Total staked balance for first pool {}", yton(emulator.contract.rewards_staked_staking_pool.total_staked_balance));

        let b_withdraw_amount = emulator.contract.get_account_unstaked_balance(b()).0;
        emulator.update_context(b(), 0);
        emulator.contract.withdraw_all();
        emulator.amount -= b_withdraw_amount;

        emulator.update_context(d(), 0);
        
        println!("D rewards {}", emulator.contract.get_account_not_staked_rewards(d()).0);
        let mut rewards_for_d = emulator.contract.get_account_not_staked_rewards(d()).0;
        assert!(rewards_for_d > 0);
        emulator.contract.withdraw_rewards(d());
        assert_eq!(emulator.contract.get_account_not_staked_rewards(d()).0, 0);

        let d_staked_amount = emulator.contract.get_account_staked_balance(d()).0;
        emulator.contract.unstake_all();
        let d_total_rewards = emulator.contract.rewards_not_staked_staking_pool.reward_per_token.multiply(d_staked_amount) - ntoy(20);

        rewards = ntoy(10);
        for _ in 0..4{
            emulator.skip_epochs_and_set_reward(1, rewards);
            emulator.distribute_rewards_between_pools(&mut future_rewards, rewards, 3);
            emulator.pay_rewards_on_epoch(&future_rewards);
            emulator.contract.ping();  
            println!("Rewards for withdraw for user d : {}", emulator.contract.get_account_not_staked_rewards(d()).0);   
        }

        emulator.update_context(d(), 0);

        emulator.transfer_from_locked_amount_to_contract_balance(d_staked_amount);
        emulator.update_context(d(), 0);
        rewards_for_d += emulator.contract.get_account_not_staked_rewards(d()).0;

        emulator.contract.withdraw_all();
        assert_eq!(emulator.contract.get_account_not_staked_rewards(d()).0, 0);
        assert_eq!(d_total_rewards, rewards_for_d);

    }

    #[test]
    fn test_farm_different_staking_pools() {
        let mut emulator = Emulator::new(
            owner(),
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7"
                .parse()
                .unwrap(),
            zero_fee(),
            Some(ntoy(100) + 10u128.pow(12)),
        );
        emulator.update_context(owner(), 0);
        emulator.contract.add_authorized_farm_token(&bob());
        let farm_amount = ntoy(100);
        add_farm(&mut emulator, farm_amount);

        emulator.deposit_and_stake(alice(), ntoy(100));

        let farm = emulator.contract.get_farm(0);
        assert_eq!(farm.name, "test".to_string());
        assert_eq!(farm.token_id, bob());
        assert_eq!(farm.start_date.0, 0);
        assert_eq!(farm.end_date.0, ONE_EPOCH_TS * 4);
        assert_eq!(farm.amount.0, ntoy(100));

        assert_eq!(emulator.contract.get_unclaimed_reward(alice(), 0).0, 0);

        let mut staking_rewards = ntoy(100);
        emulator.skip_epochs_and_set_reward(1, staking_rewards);
        emulator.update_context(alice(), 0);

        // First user got 1/8 of the rewards after 1/4 of the time.
        let alice_reward = emulator.contract.get_unclaimed_reward(alice(), 0).0;
        let mut alice_expected_farming_reward = farm_amount/8;
        assert!(almost_equal(
            alice_reward,
            alice_expected_farming_reward,
            ntoy(1) / 100
        ));

        // Adding second user.
        emulator.deposit_and_stake_rewards_not_stake(charlie(), ntoy(200));

        staking_rewards = ntoy(0);
        emulator.skip_epochs_and_set_reward(1, staking_rewards);
        emulator.contract.ping();

        println!("{} staked amount is {}", alice(), emulator.contract.get_account_staked_balance(alice()).0);
        alice_expected_farming_reward += farm_amount / 4 / 5 * 3 / 2;
        assert!(almost_equal(
            emulator.contract.get_unclaimed_reward(alice(), 0).0,
            alice_expected_farming_reward,
            ntoy(1) / 100
        ));
        let charlie_farmed = emulator.contract.get_unclaimed_reward(charlie(), 0).0;
        assert!(almost_equal(
            charlie_farmed,
            farm_amount / 4 / 5 * 2,
            ntoy(1) / 100
        ));

        emulator.deposit_and_stake(charlie(), ntoy(200));

        // Amount is still the same after depositing more without incrementing time.
        assert_eq!(
            emulator.contract.get_unclaimed_reward(charlie(), 0).0,
            charlie_farmed
        );

        emulator.skip_epochs_and_set_reward(1, 0);
        emulator.contract.ping();
        assert!(almost_equal(
            emulator.contract.get_unclaimed_reward(charlie(), 0).0,
            charlie_farmed + (farm_amount / 4 * 4 / 7),
            ntoy(1) / 100,
        ));

        emulator.update_context(alice(), 1);
        emulator.contract.claim(bob(), None);
        assert_eq!(emulator.contract.get_unclaimed_reward(alice(), 0).0, 0);
        emulator.contract.unstake_all();
        emulator.simulate_stake_call();

        assert_eq!(emulator.contract.get_account_staked_balance(alice()).0, 0);
        emulator.skip_epochs_and_set_reward(1, 0);
        assert_eq!(emulator.contract.get_account_staked_balance(charlie()). 0, ntoy(400));
        assert!(almost_equal(emulator.contract.get_unclaimed_reward(charlie(), 0).0, charlie_farmed + (farm_amount / 4 * 4 / 7) + farm_amount/4 / 11 * 8, ntoy(1) / 100));
    }

    #[test]
    fn test_migration(){
        let mut old_state = OldVersionStakingContract::new(
            "mario".parse().unwrap(), 
            "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7".parse().unwrap(),
            ntoy(100));
        let mut acc = old_state.rewards_staked_staking_pool.internal_get_account(&alice());
        acc.unstaked = 100;
        acc.stake_shares = 50;
        old_state.rewards_staked_staking_pool.internal_save_account(&alice(), &acc);
        let mut acc_rew =  OldStateAccountWithReward { unstaked: 20, stake: 50, unstaked_available_epoch_height: 0, reward_tally: 0, tally_below_zero: false, last_farm_reward_per_share: HashMap::new(), amounts:HashMap::new() };

        old_state.rewards_not_staked_staking_pool.accounts.insert(&bob(), &acc_rew);
        acc_rew.unstaked += 10;
        old_state.rewards_not_staked_staking_pool.accounts.insert(&a(), &acc_rew);

        let mut this = StakingContract{
            stake_public_key: old_state.stake_public_key,
            last_epoch_height: old_state.last_epoch_height,
            last_balance_in_contract: env::account_balance(),
            optimistic_expected_tokens: UnorderedMap::new(StorageKeys::OptimisticTimeExpectTokens),
            last_total_balance: old_state.last_total_balance,
            reward_fee_fraction: old_state.reward_fee_fraction,
            burn_fee_fraction: old_state.burn_fee_fraction,
            farms: old_state.farms,
            active_farms: old_state.active_farms,
            paused: old_state.paused,
            authorized_users: old_state.authorized_users,
            authorized_farm_tokens: old_state.authorized_farm_tokens,
            rewards_staked_staking_pool: old_state.rewards_staked_staking_pool,
            rewards_not_staked_staking_pool: InnerStakingPoolWithoutRewardsRestaked::new(),
            account_pool_register: old_state.account_pool_register,
        };
        log!("2");
        this.rewards_not_staked_staking_pool.reward_per_token = Fraction::new(old_state.rewards_not_staked_staking_pool.reward_per_token.numerator, old_state.rewards_not_staked_staking_pool.reward_per_token.denominator);
        this.rewards_not_staked_staking_pool.total_buffered_rewards = 0;
        this.rewards_not_staked_staking_pool.total_staked_balance = old_state.rewards_not_staked_staking_pool.total_staked_balance;
        this.rewards_not_staked_staking_pool.total_rewards = this.rewards_not_staked_staking_pool.reward_per_token.multiply(this.rewards_not_staked_staking_pool.total_staked_balance);
        
        let old_state_acc_vec_iter = old_state.rewards_not_staked_staking_pool.accounts.iter();
        log!("3");
        for element in old_state_acc_vec_iter{
            log!("{}", element.0);
            let acc: AccountWithReward = AccountWithReward { 
                unstaked: element.1.unstaked, 
                stake: element.1.stake, 
                unstaked_available_epoch_height: element.1.unstaked_available_epoch_height, 
                reward_tally: element.1.reward_tally, 
                tally_below_zero: element.1.tally_below_zero, 
                payed_reward: 0, 
                last_farm_reward_per_share: element.1.last_farm_reward_per_share.clone(), 
                amounts: element.1.amounts.clone() 
            };

            //log!("4 {} {}", element.0, acc.);
            this.rewards_not_staked_staking_pool.accounts.insert(&element.0, &acc);
        }

        log!("5");
    }

}
