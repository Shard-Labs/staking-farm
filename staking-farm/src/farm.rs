use near_contract_standards::fungible_token::core_impl::ext_fungible_token;
use near_sdk::{assert_one_yocto, is_promise_success, promise_result_as_success, Timestamp};

use crate::account::AccountImpl;
use crate::stake::ext_self;
use crate::staking_pool::{StakingPool};
use crate::*;

const SESSION_INTERVAL: u64 = 1_000_000_000;
const DENOMINATOR: u128 = 1_000_000_000_000_000_000_000_000;

/// Amount of gas for fungible token transfers.
pub const GAS_FOR_FT_TRANSFER: Gas = Gas(10_000_000_000_000);
/// hotfix_insuffient_gas_for_mft_resolve_transfer, increase from 5T to 20T
pub const GAS_FOR_RESOLVE_TRANSFER: Gas = Gas(20_000_000_000_000);
/// Gas for calling `get_owner` method.
pub const GAS_FOR_GET_OWNER: Gas = Gas(10_000_000_000_000);
pub const GAS_LEFTOVERS: Gas = Gas(20_000_000_000_000);
/// Get owner method on external contracts.
pub const GET_OWNER_METHOD: &str = "get_owner_account_id";

#[derive(BorshSerialize, BorshDeserialize, Clone, Debug, Serialize, Deserialize)]
#[serde(crate = "near_sdk::serde")]
pub struct RewardDistribution {
    pub undistributed: Balance,
    /// DEPRECATED: Unused.
    pub _deprecated_unclaimed: Balance,
    pub reward_per_share: U256,
    pub reward_per_share_for_accounts_not_staking_rewards: U256,
    pub reward_round: u64,
}

#[derive(BorshSerialize, BorshDeserialize, Serialize, Deserialize, Clone)]
#[serde(crate = "near_sdk::serde")]
pub struct Farm {
    pub name: String,
    pub token_id: AccountId,
    pub amount: Balance,
    pub start_date: Timestamp,
    pub end_date: Timestamp,
    pub last_distribution: RewardDistribution,
}

impl Farm {
    pub fn is_active(&self) -> bool {
        self.last_distribution.undistributed > 0
    }
}

impl StakingContract {
    pub(crate) fn internal_deposit_farm_tokens(
        &mut self,
        token_id: &AccountId,
        name: String,
        amount: Balance,
        start_date: Timestamp,
        end_date: Timestamp,
    ) {
        assert!(start_date >= env::block_timestamp(), "ERR_FARM_TOO_EARLY");
        assert!(end_date > start_date + SESSION_INTERVAL, "ERR_FARM_DATE");
        assert!(amount > 0, "ERR_FARM_AMOUNT_NON_ZERO");
        assert!(
            amount / ((end_date - start_date) / SESSION_INTERVAL) as u128 > 0,
            "ERR_FARM_AMOUNT_TOO_SMALL"
        );
        self.farms.push(&Farm {
            name,
            token_id: token_id.clone(),
            amount,
            start_date,
            end_date,
            last_distribution: RewardDistribution {
                undistributed: amount,
                _deprecated_unclaimed: 0,
                reward_per_share: U256::zero(),
                reward_per_share_for_accounts_not_staking_rewards: U256::zero(),
                reward_round: 0,
            },
        });
        self.active_farms.push(self.farms.len() - 1);
    }

    pub(crate) fn internal_add_farm_tokens(
        &mut self,
        token_id: &AccountId,
        farm_id: u64,
        additional_amount: Balance,
        start_date: Option<Timestamp>,
        end_date: Timestamp,
    ) {
        let mut farm = self.internal_get_farm(farm_id);
        assert_eq!(&farm.token_id, token_id, "ERR_FARM_INVALID_TOKEN_ID");
        assert!(additional_amount > 0, "ERR_FARM_AMOUNT_NON_ZERO");

        if let Some(distribution) = self.internal_calculate_distribution(
            &farm,
        ) {
            // The farm has started.
            assert!(start_date.is_none(), "ERR_FARM_HAS_STARTED");
            farm.amount = distribution.undistributed + additional_amount;
            farm.start_date = env::block_timestamp();
            farm.last_distribution = RewardDistribution {
                undistributed: farm.amount,
                _deprecated_unclaimed: 0,
                reward_per_share: distribution.reward_per_share,
                reward_per_share_for_accounts_not_staking_rewards: distribution.reward_per_share_for_accounts_not_staking_rewards,
                reward_round: 0,
            };
        } else {
            // The farm hasn't started, we can replace the farm.
            let start_date = start_date.unwrap_or(farm.start_date);
            assert!(start_date >= env::block_timestamp(), "ERR_FARM_TOO_EARLY");
            farm.amount += additional_amount;
            farm.start_date = start_date;
            farm.last_distribution = RewardDistribution {
                undistributed: farm.amount,
                _deprecated_unclaimed: 0,
                reward_per_share: U256::zero(),
                reward_per_share_for_accounts_not_staking_rewards: U256::zero(),
                reward_round: 0,
            };
        }
        farm.end_date = end_date;

        assert!(
            farm.end_date > farm.start_date + SESSION_INTERVAL,
            "ERR_FARM_DATE"
        );
        assert!(farm.amount > 0, "ERR_FARM_AMOUNT_NON_ZERO");
        assert!(
            farm.amount / ((farm.end_date - farm.start_date) / SESSION_INTERVAL) as u128 > 0,
            "ERR_FARM_AMOUNT_TOO_SMALL"
        );

        self.farms.replace(farm_id, &farm);
    }

    pub(crate) fn internal_get_farm(&self, farm_id: u64) -> Farm {
        self.farms.get(farm_id).expect("ERR_NO_FARM")
    }

    fn internal_calculate_distribution(
        &self,
        farm: &Farm
    ) -> Option<RewardDistribution> {
        if farm.start_date > env::block_timestamp() {
            // Farm hasn't started.
            return None;
        }
        let rewards_staked_total_staked_shares = self.rewards_staked_staking_pool.total_stake_shares - self.rewards_staked_staking_pool.total_burn_shares;
        let total_staked_amount = self.rewards_staked_staking_pool.total_staked_balance + self.rewards_not_staked_staking_pool.total_staked_balance;
        let mut distribution = farm.last_distribution.clone();
        if distribution.undistributed == 0 {
            // Farm has ended.
            return Some(distribution);
        }
        distribution.reward_round = (env::block_timestamp() - farm.start_date) / SESSION_INTERVAL;
        let reward_per_session =
            farm.amount / (farm.end_date - farm.start_date) as u128 * SESSION_INTERVAL as u128;
        let mut reward_added = (distribution.reward_round - farm.last_distribution.reward_round)
            as u128
            * reward_per_session;
        if farm.last_distribution.undistributed < reward_added {
            // Last step when the last tokens are getting distributed.
            reward_added = farm.last_distribution.undistributed;
            let increase_reward_round = (reward_added / reward_per_session) as u64;
            distribution.reward_round = farm.last_distribution.reward_round + increase_reward_round;
            if increase_reward_round as u128 * reward_per_session < reward_added {
                // Fix the rounding.
                distribution.reward_round += 1;
            }
        }
        distribution.undistributed -= reward_added;
        if rewards_staked_total_staked_shares == 0 {
            distribution.reward_per_share = U256::zero();
        } else {
            distribution.reward_per_share = 
                farm.last_distribution.reward_per_share
                + U256::from(reward_added) 
                * U256::from(DENOMINATOR) 
                / U256::from(rewards_staked_total_staked_shares) 
                * U256::from(self.rewards_staked_staking_pool.total_staked_balance) 
                / U256::from(total_staked_amount);
        }

        if self.rewards_not_staked_staking_pool.total_staked_balance == 0{
            distribution.reward_per_share_for_accounts_not_staking_rewards = U256::zero();
        }else{
            distribution.reward_per_share_for_accounts_not_staking_rewards = 
                farm.last_distribution.reward_per_share_for_accounts_not_staking_rewards
                + U256::from(reward_added) 
                * U256::from(DENOMINATOR) 
                / U256::from(self.rewards_not_staked_staking_pool.total_staked_balance) 
                * U256::from(self.rewards_not_staked_staking_pool.total_staked_balance) 
                / U256::from(total_staked_amount);
        }

        Some(distribution)
    }

    pub(crate) fn internal_unclaimed_balance(
        &self,
        account: &dyn AccountImpl,
        farm_id: u64,
        farm: &mut Farm,
        does_account_restake_rewards: bool,
    ) -> (U256, Balance) {
        if let Some(distribution) = self.internal_calculate_distribution(
            &farm
        ) {
            if distribution.reward_round != farm.last_distribution.reward_round {
                farm.last_distribution = distribution.clone();
            }
            if !account.is_burn_account() {
                let user_rps = account.get_last_reward_per_share(farm_id);

                if does_account_restake_rewards{
                    return (
                        farm.last_distribution.reward_per_share,
                        (U256::from(account.get_account_stake_shares()) * (distribution.reward_per_share - user_rps)
                            / DENOMINATOR)
                            .as_u128(),
                    )
                }else{
                    return (
                        farm.last_distribution.reward_per_share_for_accounts_not_staking_rewards,
                        (U256::from(account.get_account_stake_shares()) * (distribution.reward_per_share_for_accounts_not_staking_rewards - user_rps)
                            / DENOMINATOR)
                            .as_u128(),
                    )
                }
            }
        }
            
        (U256::zero(), 0)
    }

    fn internal_distribute_reward(
        &mut self,
        account: &mut dyn AccountImpl,
        farm_id: u64,
        mut farm: &mut Farm,
        does_account_restake_rewards: bool,
    ) {
        let (new_user_rps, claim_amount) =
            self.internal_unclaimed_balance(account, farm_id, &mut farm, does_account_restake_rewards);
        
        if !account.is_burn_account() {
            account.update_last_farm_reward_per_share(farm_id, new_user_rps);
            account.update_farm_amounts(farm.token_id.clone(), claim_amount);

            env::log_str(&format!(
                "Record {} {} reward from farm #{}",
                claim_amount, farm.token_id, farm_id
            ));
        }
    }

    /// Distribute all rewards for the given user.
    pub(crate) fn internal_distribute_all_rewards(
            &mut self, 
            account: &mut dyn AccountImpl, 
            does_account_restake_rewards: bool) 
    {
        let old_active_farms = self.active_farms.clone();
        self.active_farms = vec![];
        for farm_id in old_active_farms.into_iter() {
            if let Some(mut farm) = self.farms.get(farm_id) {
                self.internal_distribute_reward(account, farm_id, &mut farm, does_account_restake_rewards);
                self.farms.replace(farm_id, &farm);
                // TODO: currently all farms continue to be active.
                // if farm.is_active() {
                self.active_farms.push(farm_id);
                // }
            }
        }
    }

    fn internal_user_token_deposit(
        &mut self,
        account_id: &AccountId,
        token_id: &AccountId,
        amount: Balance,
    ) {
        let staking_pool = self.get_staking_pool_or_create(account_id, true);
        let mut account = staking_pool.get_account_impl(&account_id);
        account.update_farm_amounts(token_id.clone(), amount);
        staking_pool.save_account(account_id, account.as_ref());
    }

    fn internal_claim(
        &mut self,
        token_id: &AccountId,
        claim_account_id: &AccountId,
        send_account_id: &AccountId,
    ) -> Promise {
        let account_staking_rewards = self.does_account_stake_his_rewards(claim_account_id);
        let amount: Balance;
        if account_staking_rewards {
            let mut account = self.rewards_staked_staking_pool.get_account_impl(claim_account_id);
            self.internal_distribute_all_rewards(account.as_mut(), true);
            amount = account.remove_farm_amount(token_id);
            assert!(amount > 0, "ERR_ZERO_AMOUNT");
            self.rewards_staked_staking_pool.save_account(claim_account_id, account.as_ref());
        } else {
            let mut account = self.rewards_not_staked_staking_pool.get_account_impl(claim_account_id);
            self.internal_distribute_all_rewards(account.as_mut(), false);
            amount = account.remove_farm_amount(token_id);
            assert!(amount > 0, "ERR_ZERO_AMOUNT");
            self.rewards_not_staked_staking_pool.save_account(claim_account_id, account.as_ref());
        }
        env::log_str(&format!(
            "{} receives {} of {} from {}",
            send_account_id, amount, token_id, claim_account_id
        ));
        ext_fungible_token::ft_transfer(
            send_account_id.clone(),
            U128(amount),
            None,
            token_id.clone(),
            1,
            GAS_FOR_FT_TRANSFER,
        )
        .then(ext_self::callback_post_withdraw_reward(
            token_id.clone(),
            // Return funds to the account that was deducted from vs caller.
            claim_account_id.clone(),
            U128(amount),
            env::current_account_id(),
            0,
            GAS_FOR_RESOLVE_TRANSFER,
        ))
    }
}

#[near_bindgen]
impl StakingContract {
    /// Callback after checking owner for the delegated claim.
    #[private]
    pub fn callback_post_get_owner(
        &mut self,
        token_id: AccountId,
        delegator_id: AccountId,
        account_id: AccountId,
    ) -> Promise {
        let owner_id: AccountId = near_sdk::serde_json::from_slice(
            &promise_result_as_success().expect("get_owner must have result"),
        )
        .expect("Failed to parse");
        assert_eq!(owner_id, account_id, "Caller is not an owner");
        self.internal_claim(&token_id, &delegator_id, &account_id)
    }

    /// Callback from depositing funds to the user's account.
    /// If it failed, return funds to the user's account.
    #[private]
    pub fn callback_post_withdraw_reward(
        &mut self,
        token_id: AccountId,
        sender_id: AccountId,
        amount: U128,
    ) {
        if !is_promise_success() {
            // This reverts the changes from the claim function.
            self.internal_user_token_deposit(&sender_id, &token_id, amount.0);
            env::log_str(&format!(
                "Returned {} {} to {}",
                amount.0, token_id, sender_id
            ));
        }
    }

    #[private]
    pub fn callback_post_stop_farm_transfer_tokens(
        &mut self,
        farm_id: u64,
        farm: Farm,
    ){
        // if transfer of tokens was not successful, replace the farm data with the passed farm,
        // the farm that was sent should be in the initial state
        if !is_promise_success(){
            self.farms.replace(farm_id, &farm);    
        }
    }

    /// Claim given tokens for given account.
    /// If delegator is provided, it will call it's `get_owner` method to confirm that caller
    /// can execute on behalf of this contract.
    /// - Requires one yoctoNEAR. To pass to the ft_transfer call and to guarantee the full access key.
    #[payable]
    pub fn claim(&mut self, token_id: AccountId, delegator_id: Option<AccountId>) -> Promise {
        assert_one_yocto();
        let account_id = env::predecessor_account_id();
        if let Some(delegator_id) = delegator_id {
            Promise::new(delegator_id.clone())
                .function_call(GET_OWNER_METHOD.to_string(), vec![], 0, GAS_FOR_GET_OWNER)
                .then(ext_self::callback_post_get_owner(
                    token_id,
                    delegator_id,
                    account_id,
                    env::current_account_id(),
                    0,
                    env::prepaid_gas() - env::used_gas() - GAS_FOR_GET_OWNER - GAS_LEFTOVERS,
                ))
        } else {
            self.internal_claim(&token_id, &account_id, &account_id)
        }
    }

    /// functionality for removing already stopped farm
    /// to be able to add new farm
    #[payable]
    pub fn remove_stopped_farm(&mut self, farm_id: u64){
        self.assert_owner();
        let farm = self.internal_get_farm(farm_id);
        assert_eq!(farm.is_active(), false, "Farm should be inactive");

        let farm_id_idx = self.active_farms.iter().position(|el| *el == farm_id).expect("ERR_NO_FARM");
        self.active_farms.remove(farm_id_idx);
    }

    /// Stops given farm at the current moment.
    /// Warning: IF OWNER ACCOUNT DOESN'T HAVE STORAGE, THESE FUNDS WILL BE STUCK ON THE STAKING FARM.
    #[payable]
    pub fn stop_farm(&mut self, farm_id: u64) -> Promise {
        self.assert_owner();
        let mut farm = self.internal_get_farm(farm_id);
        let farm_initial_state: Farm = farm.clone();
        let leftover_amount = if let Some(distribution) = self.internal_calculate_distribution(
            &farm,
        ) {
            farm.end_date = env::block_timestamp();
            farm.last_distribution = distribution;
            farm.last_distribution.undistributed
        } else {
            farm.amount
        };
        farm.amount -= leftover_amount;
        farm.last_distribution.undistributed = 0;
        self.farms.replace(farm_id, &farm);

        ext_fungible_token::ft_transfer(
            StakingContract::internal_get_owner_id(),
            U128(leftover_amount),
            None,
            farm.token_id.clone(),
            1,
            GAS_FOR_FT_TRANSFER,
        ).then(ext_self::callback_post_stop_farm_transfer_tokens(
            farm_id,
            farm_initial_state,
            env::current_account_id(),
            0,
            GAS_FOR_RESOLVE_TRANSFER,
        ))
    }
}

#[cfg(test)]
mod farm_tests {
    use super::Farm;
    use super::*;

    #[test]
    fn farms_test(){
        let mut farms = Vector::<Farm>::new(b"x".to_vec());
        let mut active_farms = Vec::<u64>::new();

        let f1 = Farm{
            name: "test 1".to_string(), 
            token_id: test_utils::alice(), 
            amount: 100, 
            start_date: 1, 
            end_date: 20,
            last_distribution: RewardDistribution { 
                undistributed: 10, 
                _deprecated_unclaimed: 0, 
                reward_per_share: U256::zero(), 
                reward_per_share_for_accounts_not_staking_rewards: U256::zero(), 
                reward_round: 1 
            }};

        farms.push(&f1);
        active_farms.push(farms.len() - 1);

        let f2 = Farm{
            name: "test 2".to_string(), 
            token_id: test_utils::bob(), 
            amount: 200, 
            start_date: 1, 
            end_date: 50,
            last_distribution: RewardDistribution { 
                undistributed: 10, 
                _deprecated_unclaimed: 0, 
                reward_per_share: U256::zero(), 
                reward_per_share_for_accounts_not_staking_rewards: U256::zero(), 
                reward_round: 1 
            }};

        farms.push(&f2);
        active_farms.push(farms.len() - 1);

        let f3 = Farm{
            name: "test 3".to_string(), 
            token_id: test_utils::charlie(), 
            amount: 300, 
            start_date: 1, 
            end_date: 1000,
            last_distribution: RewardDistribution { 
                undistributed: 10, 
                _deprecated_unclaimed: 0, 
                reward_per_share: U256::zero(), 
                reward_per_share_for_accounts_not_staking_rewards: U256::zero(), 
                reward_round: 1 
            }};

        farms.push(&f3);
        active_farms.push(farms.len() - 1);

        let f4 = Farm{
            name: "test 4".to_string(), 
            token_id: test_utils::owner(), 
            amount: 400, 
            start_date: 1, 
            end_date: 50,
            last_distribution: RewardDistribution { 
                undistributed: 10, 
                _deprecated_unclaimed: 0, 
                reward_per_share: U256::zero(), 
                reward_per_share_for_accounts_not_staking_rewards: U256::zero(), 
                reward_round: 1 
            }};

        farms.push(&f4);
        active_farms.push(farms.len() - 1);

        assert_eq!(farms.get(0).unwrap().name, f1.name);
        assert_eq!(farms.get(3).unwrap().name, f4.name);

        print_farms(&active_farms, &farms);

        // remove farm with id 1, this is farm f2
        let farm_idx = active_farms.iter().position(|x| *x == 1).unwrap();
        active_farms.remove(farm_idx);
        
        assert_eq!(active_farms.len(), 3);
        assert_eq!(farms.len(), 4);

        print_farms(&active_farms, &farms);

        farms.push(&f2);
        active_farms.push(farms.len() - 1);

        assert_eq!(farms.get(4).unwrap().name, f2.name);

        print_farms(&active_farms, &farms);

        let farm_idx = active_farms.iter().position(|x| *x == 4).expect("ERR_NO_FARM");
        active_farms.remove(farm_idx);

        assert_eq!(active_farms.len(), 3);
        assert_eq!(farms.len(), 5);

        print_farms(&active_farms, &farms);
    }
    fn print_farms(active_farms: &Vec<u64>, farms: &Vector<Farm>){
        let ff: Vec<HumanReadableFarm> = active_farms
        .iter()
        .map(|&index| HumanReadableFarm::from(index, farms.get(index).unwrap()))
        .collect();

        println!("------------------");
        ff.iter().for_each(|el| println!("id: {} name: {}", el.farm_id, el.name));
    }
}