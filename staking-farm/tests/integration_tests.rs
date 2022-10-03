use anyhow::Ok;
use near_sdk::{AccountId, serde_json::json, Balance};
use near_sdk_sim::{DEFAULT_GAS, to_yocto};
use staking_farm::{Ratio, STAKE_SHARE_PRICE_GUARANTEE_FUND};
use workspaces::{Worker, network::{Sandbox, NetworkInfo, AllowDevAccountCreation, NetworkClient}, prelude::{DevAccountDeployer, TopLevelAccountCreator}, Contract, Account};

near_sdk_sim::lazy_static_include::lazy_static_include_bytes! {
    STAKING_FARM_BYTES => "../res/staking_farm_release.wasm",
    TEST_TOKEN_BYTES => "../res/test_token.wasm",
    WHITELIST_BYTES => "../res/whitelist.wasm",
    LOCKUP_BYTES => "../res/lockup_contract.wasm",
}

const STAKING_POOL_ACCOUNT_ID: &str = "pool";
const STAKING_KEY: &str = "KuTCtARNzxZQ3YvXDeLjx83FDqxv2SdQTSbiq876zR7";
const ONE_SEC_IN_NS: u64 = 1_000_000_000;
const WHITELIST_ACCOUNT_ID: &str = "whitelist";
const ROOT_ACC: &str = "root.near";
const LOCKUP_ACCOUNT_ID: &str = "lockup";
const ZERO_ADDRESS: &str = "0000000000000000000000000000000000000000000000000000000000000000";


async fn deploy_and_init_whitelist<T>(worker: &Worker<T>) -> anyhow::Result<Contract>
where T:
        TopLevelAccountCreator 
        + NetworkInfo 
        + AllowDevAccountCreation 
        + Send 
        + Sync
        + NetworkClient{
    let whitelist = worker.dev_deploy(&WHITELIST_BYTES).await?;

    whitelist
        .call(&worker, "new")
        .args_json(json!({ "foundation_account_id": "root" }))?
        .gas(DEFAULT_GAS)
        //.deposit(to_yocto("10"))
        .transact()
        .await?;

    return Ok(whitelist);
}

async fn deploy_and_init_token<T>(worker: &Worker<T>) -> anyhow::Result<Contract>
    where T:
            TopLevelAccountCreator 
            + NetworkInfo 
            + AllowDevAccountCreation 
            + Send 
            + Sync
            + NetworkClient{
    let token = worker.dev_deploy(&TEST_TOKEN_BYTES).await?;
    token
        .call(&worker, "new")
        .gas(DEFAULT_GAS)
        .transact()
        .await?;
    
    return Ok(token);
}

async fn deploy_and_init_lockup<T>(worker: &Worker<T>, whitelist_acc: &workspaces::AccountId, owner_id: &workspaces::AccountId) -> anyhow::Result<Contract>
    where T:
            TopLevelAccountCreator 
            + NetworkInfo 
            + AllowDevAccountCreation 
            + Send 
            + Sync
            + NetworkClient{
    let lockup = worker.dev_deploy(&LOCKUP_BYTES).await?;
    lockup
        .call(&worker, "new")
        .args_json(json!({ "owner_account_id": owner_id, "lockup_duration": "100000000000000", "transfers_information": { "TransfersEnabled": { "transfers_timestamp": "0" } }, "staking_pool_whitelist_account_id": whitelist_acc }))?
        .gas(DEFAULT_GAS)
        //.deposit(to_yocto(""))
        .transact()
        .await?;
    
    return Ok(lockup);
}

async fn deploy_and_init_staking_contract<T>(worker: &Worker<T>, 
        owner: &workspaces::Account, 
        reward_fee_fraction: Ratio, 
        burn_fee: Ratio, 
        initial_balance: Balance
    ) -> anyhow::Result<Contract>
    where T:
            TopLevelAccountCreator 
            + NetworkInfo 
            + AllowDevAccountCreation 
            + Send 
            + Sync
            + NetworkClient{

    let contract_acc = owner
        .create_subaccount(&worker, "s1")
        .initial_balance(initial_balance)
        .transact()
        .await?
        .unwrap();

    let contract = contract_acc.deploy(&worker, &STAKING_FARM_BYTES).await?.unwrap();

    contract
        .call(&worker, "new")
        .args_json(json!({ "owner_id": contract_acc.id(), "stake_public_key": STAKING_KEY, "reward_fee_fraction": reward_fee_fraction, "burn_fee_fraction": burn_fee }))?
        .gas(DEFAULT_GAS)
        .transact()
        .await?;

    println!("{}", contract.as_account().view_account(&worker).await?.balance);
    
    return Ok(contract);
}

#[tokio::test]
async fn test_unstake_rewards() -> anyhow::Result<()>{
    
    let (worker, contract, root) = setup(to_yocto("50") + STAKE_SHARE_PRICE_GUARANTEE_FUND, 0, 0).await?;

    return Ok(println!("x"));
}

async fn setup(
    pool_initial_balance: near_sdk::Balance,
    reward_ratio: u32,
    burn_ratio: u32
) -> anyhow::Result<(Worker<Sandbox>, Contract, Account)>{
    let worker = workspaces::sandbox().await?;
    let whitelist = deploy_and_init_whitelist(&worker).await?;
    let token = deploy_and_init_token(&worker).await?;
    let (_, sk) = worker.dev_generate().await;
    let root_acc = worker.dev_create_account().await?;
    let ac = worker.create_tla(format!("root.{}", worker.info().root_id).parse::<workspaces::AccountId>().unwrap(), sk).await?.unwrap();
    println!("{} {}", ac.view_account(&worker).await?.balance, ac.id());
    //let root_acc = worker.cra(ROOT_ACC.parse::<workspaces::AccountId>().unwrap(), sk).await?.unwrap();

    token
        .call(&worker, "mint")
        .args_json(json!({ "account_id": root_acc.id(), "amount": to_yocto("100000").to_string() }))?
        .gas(DEFAULT_GAS)
        .transact()
        .await?;

    let lockup = deploy_and_init_lockup(&worker, whitelist.id(), root_acc.id()).await?;

    let reward_ratio = Ratio {
        numerator: reward_ratio,
        denominator: 10,
    };
    let burn_ratio = Ratio {
        numerator: burn_ratio,
        denominator: 10,
    };

    let staking_pool = deploy_and_init_staking_contract(
        &worker, 
        &root_acc, 
        reward_ratio, 
        burn_ratio, 
        pool_initial_balance).await?;

    token.call(&worker, "storage_deposit")
        .args_json(json!({ "account_id": staking_pool.id() }))?
        .gas(DEFAULT_GAS)
        .deposit(1)
        .transact()
        .await?;

    whitelist.call(&worker, "add_staking_pool")
        .args_json(json!({ "staking_pool_account_id": staking_pool.id() }))?
        .gas(DEFAULT_GAS)
        .transact()
        .await?;

    root_acc.call(&worker, staking_pool.id(), "add_authorized_farm_token")
        .args_json(json!({ "token_id": token.id() }))?
        .gas(DEFAULT_GAS)
        .transact()
        .await?;

    root_acc.call(&worker, staking_pool.id(), "add_authorized_farm_token")
        .args_json(json!({ "token_id": token.id() }))?
        .gas(DEFAULT_GAS)
        .transact()
        .await?;
    
    return Ok((worker, staking_pool, root_acc));
}
/*
async fn setup(
    pool_initial_balance: Balance,
    reward_ratio: u32,
    burn_ratio: u32,
) -> (UserAccount, PoolContract) {
    
    let worker = workspaces::sandbox().await?;
    let (worker_acc_id, sk) = worker.dev_generate().await;

    call(
        &root,
        pool.account_id(),
        "add_authorized_farm_token",
        json!({ "token_id": token_id() }),
        0,
    );
    (root, pool)
}
*/