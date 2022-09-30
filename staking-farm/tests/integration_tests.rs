use anyhow::Ok;
use near_sdk::{AccountId, serde_json::json};
use near_sdk_sim::{DEFAULT_GAS, to_yocto};
use workspaces::{Worker, network::Sandbox, prelude::{DevAccountDeployer, TopLevelAccountCreator}};

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
const ROOT_ACCOUNT_ID: &str = "root";
const LOCKUP_ACCOUNT_ID: &str = "lockup";
const ZERO_ADDRESS: &str = "0000000000000000000000000000000000000000000000000000000000000000";

#[tokio::test]
async fn test() -> anyhow::Result<()>{
    let worker = workspaces::sandbox().await?;
    let (_, sk) = worker.dev_generate().await;

    let c = worker.dev_deploy(&WHITELIST_BYTES).await?;

    c
        .call(&worker, "new")
        .args_json(json!({ "foundation_account_id": "root" }))?
        .gas(DEFAULT_GAS)
        //.deposit(to_yocto("10"))
        .transact()
        .await?;
    
    println!("Called");

    let whitelist = worker
        .create_tla_and_deploy(
            WHITELIST_ACCOUNT_ID.parse()?, 
            sk, 
            &WHITELIST_BYTES
        )
        .await?
        .result;

    // let result = whitelist
    //     .call(&worker, "new")
    //     .args_json(json!({ "foundation_account_id": "x" }))?
    //     .gas(DEFAULT_GAS)
    //     //.deposit(to_yocto("10"))
    //     .transact()
    //     .await?;

    

    return Ok(println!("x"));
}
/*
async fn setup(
    pool_initial_balance: Balance,
    reward_ratio: u32,
    burn_ratio: u32,
) -> (UserAccount, PoolContract) {
    
    let worker = workspaces::sandbox().await?;
    let (worker_acc_id, sk) = worker.dev_generate().await;

    let whitelist = worker
        .create_tla_and_deploy(
            WHITELIST_ACCOUNT_ID.parse()?, 
            sk, 
            &WHITELIST_BYTES
        )
        .await?
        .result;

    whitelist
        .call(&worker, "new")
        .args_json(json!({ "foundation_account_id": worker_acc_id }))?
        .gas(DEFAULT_GAS)
        .deposit(to_yocto("10"))
        .transact()
        .await?;


    // Disable contract rewards.
    root.borrow_runtime_mut()
        .genesis
        .runtime_config
        .transaction_costs
        .burnt_gas_reward = Rational::new(0, 1);
    let whitelist = root.deploy_and_init(
        &WHITELIST_BYTES,
        AccountId::new_unchecked(WHITELIST_ACCOUNT_ID.to_string()),
        "new",
        &serde_json::to_vec(&json!({ "foundation_account_id": root.account_id() })).unwrap(),
        to_yocto("10"),
        near_sdk_sim::DEFAULT_GAS,
    );
    let _other_token = root.deploy_and_init(
        &TEST_TOKEN_BYTES,
        token_id(),
        "new",
        &[],
        to_yocto("10"),
        near_sdk_sim::DEFAULT_GAS,
    );
    assert_all_success(root.call(
        token_id(),
        "mint",
        &serde_json::to_vec(&json!({ "account_id": root.account_id(), "amount": to_yocto("100000").to_string() })).unwrap(),
        near_sdk_sim::DEFAULT_GAS,
        0,
    ));
    let _lockup = root.deploy_and_init(
        &LOCKUP_BYTES,
        lockup_id(),
        "new",
        &serde_json::to_vec(&json!({ "owner_account_id": root.account_id(), "lockup_duration": "100000000000000", "transfers_information": { "TransfersEnabled": { "transfers_timestamp": "0" } }, "staking_pool_whitelist_account_id": WHITELIST_ACCOUNT_ID })).unwrap(),
        to_yocto("100000"),
        near_sdk_sim::DEFAULT_GAS,
    );
    let reward_ratio = Ratio {
        numerator: reward_ratio,
        denominator: 10,
    };
    let burn_ratio = Ratio {
        numerator: burn_ratio,
        denominator: 10,
    };
    let pool = deploy!(
        contract: StakingContractContract,
        contract_id: STAKING_POOL_ACCOUNT_ID.to_string(),
        bytes: &STAKING_FARM_BYTES,
        signer_account: root,
        // adding STAKE_SHARE_PRICE_GUARANTEE_FUND to remove this rounding issue from further calculations.
        deposit: pool_initial_balance,
        init_method: new(root.account_id(), STAKING_KEY.parse().unwrap(), reward_ratio, burn_ratio)
    );
    assert_all_success(root.call(
        token_id(),
        "storage_deposit",
        &serde_json::to_vec(&json!({ "account_id": pool.account_id() })).unwrap(),
        near_sdk_sim::DEFAULT_GAS,
        to_yocto("1"),
    ));
    call(
        &root,
        whitelist.account_id(),
        "add_staking_pool",
        json!({ "staking_pool_account_id": STAKING_POOL_ACCOUNT_ID }),
        0,
    );
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
#[tokio::test]
async fn test_transfer() -> anyhow::Result<()> {
    test();

    return Ok(println!("Mario"));
}