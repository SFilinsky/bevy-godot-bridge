//! Types used by `action_pipeline!`.
//!
//! An action can start in Godot or Rust and finish in Bevy. The pipeline keeps
//! the action's input, checks whether it is allowed, and runs it. Godot-created
//! actions send updates and results back to Godot. Rust-created actions use the
//! same checks and execution without needing a Godot response.

/// Whether an action report currently permits execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllowanceSummary {
    Ok,
    NotAllowed,
}

/// Godot-facing status of one check result.
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

/// A status update sent from Bevy to Godot for one action.
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

/// Result produced when an action execution completes.
#[derive(Debug, Clone)]
pub struct ExecuteResult<TPayload> {
    pub ok: bool,
    pub payload: Option<TPayload>,
}

/// Names one kind of action and the data it needs.
pub trait ActionParams: Clone + Default {
    type FullParams: Clone;

    fn to_full(&self) -> Option<Self::FullParams>;
    fn from_full(full: &Self::FullParams) -> Self;
    fn merge_from(&mut self, other: &Self);
}

/// Stable identifier for one action instance, whether Godot or Rust created it.
pub type ActionInstanceId = u64;
/// Stable identifier for one execution requested by an action instance.
pub type ExecutionId = u64;

/// Holds action IDs that need updating until the pipeline reads them.
/// The same ID is kept only once.
#[derive(Default)]
pub struct DirtyActionIdList {
    action_id_list: Vec<ActionInstanceId>,
    action_id_set: std::collections::HashSet<ActionInstanceId>,
}

impl DirtyActionIdList {
    pub fn mark(&mut self, action_instance_id: ActionInstanceId) {
        if self.action_id_set.insert(action_instance_id) {
            self.action_id_list.push(action_instance_id);
        }
    }

    pub fn drain(&mut self) -> Vec<ActionInstanceId> {
        self.action_id_set.clear();
        std::mem::take(&mut self.action_id_list)
    }
}

/// Explains why the pipeline needs to check an action again.
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

/// The answer from one rule, with optional extra data and a failure code.
#[derive(Debug, Clone, PartialEq)]
pub enum Criterion<TFail> {
    Ok { payload: Option<TFail> },
    NotProvided { payload: Option<TFail> },
    Fail {
        code: &'static str,
        payload: Option<TFail>,
    },
}

impl<TFail> Criterion<TFail> {
    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Ok { .. })
    }
}

/// Godot constants mirroring [`ActionCheckStatus`].
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

/// Godot constants mirroring [`ActionStatus`].
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

/// A rule that says whether an action is allowed.
///
/// The action pipeline asks these rules for an answer and sends the answer to
/// Godot. A check can also say when an earlier answer may be out of date.
pub trait Check {
    type Fail;

    /// Returns extra behavior when this rule can find actions that need checking again.
    fn continuous_check(&mut self) -> Option<&mut dyn ContinuousCheck> {
        None
    }
}

/// Lets a rule ask the pipeline to check an action again after something changes.
///
/// The pipeline stores the answer and sends it to Godot. This type only says
/// which actions need a new answer.
pub trait ContinuousCheck {
    /// Tells this rule that one action changed. `None` means the action was removed.
    fn apply_action_change(
        &mut self,
        action_instance_id: ActionInstanceId,
        partial_params: Option<&dyn Any>,
    );

    /// Tells this rule that shared action data changed.
    ///
    /// Most rules can ignore this. A rule that uses this data can read it and
    /// ask to recheck only the actions it affects.
    fn apply_static_data_change(&mut self, _static_data: &dyn Any) {}

    /// Returns each action that needs this check field reevaluated since the previous refresh.
    fn flush_dirty_action_id_list(&mut self) -> Vec<ActionInstanceId>;
}

/// Connects one action's data to one rule.
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

/// Lets callers read whether a report allows an action.
pub trait CheckReportLike {
    fn allowance(&self) -> AllowanceSummary;

    fn is_allowed(&self) -> bool {
        self.allowance() == AllowanceSummary::Ok
    }
}

/// Runs an action after its final checks pass.
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
use std::any::Any;
