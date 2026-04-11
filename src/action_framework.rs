#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllowanceSummary {
    Ok,
    NotAllowed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i64)]
pub enum ActionCheckStatus {
    Ok = 1,
    Failure = 2,
    NotProvided = 3,
}

impl ActionCheckStatus {
    pub fn as_i64(self) -> i64 {
        self as i64
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i64)]
pub enum ActionStatus {
    Checked = 1,
    ExecutionSuccess = 2,
    ExecutionFailed = 3,
}

impl ActionStatus {
    pub fn as_i64(self) -> i64 {
        self as i64
    }
}

#[derive(Debug, Clone)]
pub struct ExecuteResult<TPayload> {
    pub ok: bool,
    pub payload: TPayload,
}

pub trait ActionParams: Clone + Default {
    type FullParams: Clone;

    fn to_full(&self) -> Option<Self::FullParams>;
    fn from_full(full: &Self::FullParams) -> Self;
    fn merge_from(&mut self, other: &Self);
}

pub type ActionInstanceId = u64;
pub type ExecutionId = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CheckReason {
    AsyncCheck,
    UpdatedParamsCheck,
    ExecuteCheck,
}

impl CheckReason {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::AsyncCheck => "async_check",
            Self::UpdatedParamsCheck => "updated_params_check",
            Self::ExecuteCheck => "execute_check",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Criterion<TFail> {
    Ok,
    NotProvided,
    Fail {
        code: &'static str,
        payload: Option<TFail>,
    },
}

impl<TFail> Criterion<TFail> {
    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Ok)
    }
}

#[derive(GodotClass)]
#[class(init, base=RefCounted)]
pub struct ActionCheckStatusCodes {
    #[base]
    base: Base<RefCounted>,
}

#[godot_api]
impl ActionCheckStatusCodes {
    #[constant]
    const OK: i64 = ActionCheckStatus::Ok as i64;
    #[constant]
    const FAILURE: i64 = ActionCheckStatus::Failure as i64;
    #[constant]
    const NOT_PROVIDED: i64 = ActionCheckStatus::NotProvided as i64;
}

#[derive(GodotClass)]
#[class(init, base=RefCounted)]
pub struct ActionStatusCodes {
    #[base]
    base: Base<RefCounted>,
}

#[godot_api]
impl ActionStatusCodes {
    #[constant]
    const CHECKED: i64 = ActionStatus::Checked as i64;
    #[constant]
    const EXECUTION_SUCCESS: i64 = ActionStatus::ExecutionSuccess as i64;
    #[constant]
    const EXECUTION_FAILED: i64 = ActionStatus::ExecutionFailed as i64;
}

pub trait Check {
    type Fail;
}

pub trait CheckAdapter {
    type PartialParams;
    type StaticData;
    type CheckSubsystem<'w, 's>: Check;

    fn map_and_check<'w, 's>(
        partial_params: &Self::PartialParams,
        static_data: &Self::StaticData,
        subsystem: &mut Self::CheckSubsystem<'w, 's>,
    ) -> Criterion<<Self::CheckSubsystem<'w, 's> as Check>::Fail>;
}

pub trait CheckReportLike {
    fn allowance(&self) -> AllowanceSummary;

    fn is_allowed(&self) -> bool {
        self.allowance() == AllowanceSummary::Ok
    }
}

pub trait ExecuteAction<FullParams, ExecuteResult> {
    type StaticData: crate::dto::DataTransferConfig<DataType = Self::StaticData>;

    fn execute_action(
        &mut self,
        static_data: &Self::StaticData,
        params: FullParams,
    ) -> ExecuteResult;
}
use godot::classes::RefCounted;
use godot::obj::Base;
use godot::prelude::{godot_api, GodotClass};
