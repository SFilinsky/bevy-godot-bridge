//! Code used by `action_pipeline!`.
//!
//! An action pipeline lets Godot ask Bevy to do something, such as building a
//! structure. It remembers the action, checks whether it is allowed, runs it,
//! and sends the result back to Godot. It is not for simple one-time messages
//! such as "gold was added".

use heck::{ToSnakeCase, ToUpperCamelCase};
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, Path, Result, Token, Type,
};

struct CheckSpec {
    name: Ident,
    mapper_type: Type,
}

impl Parse for CheckSpec {
    fn parse(input: ParseStream) -> Result<Self> {
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let mapper_type: Type = input.parse()?;
        Ok(Self { name, mapper_type })
    }
}

struct Spec {
    name: Ident,
    partial_params: Type,
    execute_result_payload: Type,
    checks: Vec<CheckSpec>,
    execute_subsystem: Path,
    action_set: Path,
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut name: Option<Ident> = None;
        let mut partial_params: Option<Type> = None;
        let mut execute_result_payload: Option<Type> = None;
        let mut checks: Option<Vec<CheckSpec>> = None;
        let mut execute_subsystem: Option<Path> = None;
        let mut action_set: Option<Path> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match key.to_string().as_str() {
                "name" => name = Some(input.parse()?),
                "partial_params" => partial_params = Some(input.parse()?),
                "execute_result_payload" => execute_result_payload = Some(input.parse()?),
                "checks" => {
                    let content;
                    bracketed!(content in input);
                    let list = content
                        .parse_terminated(CheckSpec::parse, Token![,])?
                        .into_iter()
                        .collect::<Vec<_>>();
                    checks = Some(list);
                }
                "execute_subsystem" => execute_subsystem = Some(input.parse()?),
                "action_set" => action_set = Some(input.parse()?),
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!(
                            "Unknown key `{other}`; expected `name`, `partial_params`, `execute_result_payload`, `checks`, `execute_subsystem`, `action_set`"
                        ),
                    ));
                }
            }

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }
        }

        let name = name.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "action_pipeline!: missing `name: ActionName`",
            )
        })?;
        let partial_params = partial_params.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "action_pipeline!: missing `partial_params: Type`",
            )
        })?;
        let execute_result_payload = execute_result_payload.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "action_pipeline!: missing `execute_result_payload: Type`",
            )
        })?;
        let checks = checks.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "action_pipeline!: missing `checks: [name: MapperType, ...]`",
            )
        })?;
        let execute_subsystem = execute_subsystem.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "action_pipeline!: missing `execute_subsystem: path::ToSubsystem`",
            )
        })?;
        let action_set = action_set.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "action_pipeline!: missing `action_set: path::ToSet`",
            )
        })?;

        Ok(Self {
            name,
            partial_params,
            execute_result_payload,
            checks,
            execute_subsystem,
            action_set,
        })
    }
}

pub fn expand(input: TokenStream) -> TokenStream {
    let spec = parse_macro_input!(input as Spec);
    expand_spec(spec).into()
}

fn expand_spec(spec: Spec) -> proc_macro2::TokenStream {

    let action_name_ident = spec.name;
    let action_name = action_name_ident.to_string();
    let module_name = format_ident!("{}_action_pipeline", action_name.to_snake_case());
    let plugin_name = format_ident!("{}ActionPlugin", action_name_ident);
    let subsystem_name = format_ident!("{}ActionSubsystem", action_name_ident);
    let check_subsystem_name = format_ident!("{}ActionCheckSubsystem", action_name_ident);
    let check_runner_subsystem_name =
        format_ident!("{}ActionCheckRunnerSubsystem", action_name_ident);
    let check_refresh_timing_name = format_ident!("{}ActionCheckRefreshTiming", action_name_ident);
    let instance_name = format_ident!("{}ActionInstanceDto", action_name_ident);
    let node_name = format_ident!("{}ActionNode", action_name_ident);
    let check_report_name = format_ident!("{}ActionCheckReport", action_name_ident);
    let check_report_dto_name = format_ident!("{}ActionCheckReportDto", action_name_ident);
    let check_change_name = format_ident!("{}ActionCheckChange", action_name_ident);
    let check_changes_subsystem_name =
        format_ident!("{}ActionCheckChangeSubsystem", action_name_ident);
    let status_codes_dto_name =
        format_ident!("{}ActionCheckReportStatusCodesDto", action_name_ident);
    let report_transfer_config_name =
        format_ident!("{}ActionCheckReportTransferConfig", action_name_ident);

    let partial_params = spec.partial_params;
    let execute_result_payload = spec.execute_result_payload;
    let checks = spec.checks;
    if checks.len() > 64 {
        return syn::Error::new(
            Span::call_site(),
            "action_pipeline!: at most 64 checks are supported by the cached check mask",
        )
        .to_compile_error();
    }
    let execute_subsystem = spec.execute_subsystem;
    let action_set = spec.action_set;

    let check_aliases = checks
        .iter()
        .map(|c| format_ident!("{}FailType", c.name.to_string().to_upper_camel_case()))
        .collect::<Vec<_>>();
    let check_subsystem_aliases = checks
        .iter()
        .map(|c| {
            format_ident!(
                "{}CheckSubsystemType",
                c.name.to_string().to_upper_camel_case()
            )
        })
        .collect::<Vec<_>>();

    let check_type_alias_defs = checks.iter().zip(check_aliases.iter()).map(|(c, alias)| {
        let mapper = &c.mapper_type;
        quote! {
            type #alias = <<#mapper as ::bevy_godot4::action_framework::CheckAdapter>::CheckSubsystem<'static, 'static>
                as ::bevy_godot4::action_framework::Check>::Fail;
        }
    });

    let check_subsystem_type_alias_defs = checks.iter().zip(check_subsystem_aliases.iter()).map(
        |(c, alias)| {
            let mapper = &c.mapper_type;
            quote! {
                type #alias<'w, 's> = <#mapper as ::bevy_godot4::action_framework::CheckAdapter>::CheckSubsystem<'w, 's>;
            }
        },
    );

    let check_paramset_types = check_subsystem_aliases
        .iter()
        .map(|alias| quote! { #alias<'w, 's> })
        .collect::<Vec<_>>();
    let action_subsystem_paramset_types = std::iter::once(quote! { #execute_subsystem<'w, 's> })
        .chain(std::iter::once(quote! { #check_subsystem_name<'w, 's> }))
        .collect::<Vec<_>>();

    let check_field_defs = checks.iter().zip(check_aliases.iter()).map(|(c, alias)| {
        let name = &c.name;
        quote! {
            pub #name: ::bevy_godot4::action_framework::Criterion<#alias>,
        }
    });

    let check_field_args = checks.iter().zip(check_aliases.iter()).map(|(c, alias)| {
        let name = &c.name;
        quote! {
            #name: ::bevy_godot4::action_framework::Criterion<#alias>
        }
    });

    let check_field_inits = checks.iter().map(|c| {
        let name = &c.name;
        quote! { #name, }
    });

    let check_allowance_terms = checks.iter().map(|c| {
        let name = &c.name;
        quote! { && #name.is_ok() }
    });

    let check_default_inits = checks.iter().map(|c| {
        let name = &c.name;
        quote! {
            #name: ::bevy_godot4::action_framework::Criterion::NotProvided { payload: None },
        }
    });

    let check_mask_constants = checks.iter().enumerate().map(|(index, c)| {
        let constant = format_ident!("CHECK_MASK_{}", c.name.to_string().to_upper_camel_case());
        let bit = 1_u64 << index;
        quote! { const #constant: u64 = #bit; }
    }).collect::<Vec<_>>();
    let all_check_mask = if checks.is_empty() {
        0
    } else {
        (1_u64 << checks.len()) - 1
    };
    let check_refresh_inits = checks.iter().enumerate().map(|(index, c)| {
        let name = &c.name;
        let mapper = &c.mapper_type;
        let var = format_ident!("{}_check_subsystem", c.name);
        let p_method = format_ident!("p{}", index);
        let constant = format_ident!("CHECK_MASK_{}", c.name.to_string().to_upper_camel_case());
        quote! {
            if check_mask & #constant != 0 {
                let check_started_at = ::std::time::Instant::now();
                let mut #var = self.subsystems.#p_method();
                report.#name = <#mapper as ::bevy_godot4::action_framework::CheckAdapter>::map_and_check(
                    pp,
                    static_data,
                    &mut #var,
                );
                if let Some(check_timing) = check_timing.as_deref_mut() {
                    check_timing.#name += check_started_at.elapsed();
                }
            }
        }
    }).collect::<Vec<_>>();

    let check_refresh_timing_field_defs = checks.iter().map(|c| {
        let name = &c.name;
        quote! { #name: ::std::time::Duration, }
    });

    let runner_metric_prefix = format!("{action_name} action check runner");
    let action_change_drain_metric_name =
        format!("{runner_metric_prefix}: tracked action change drain");
    let scheduling_metric_name = format!("{runner_metric_prefix}: candidate scheduling");
    let preparation_metric_name = format!("{runner_metric_prefix}: candidate preparation");
    let evaluation_metric_name = format!("{runner_metric_prefix}: candidate evaluation");
    let delivery_metric_name = format!("{runner_metric_prefix}: cached report update and delivery");
    let continuous_check_metric_names = checks.iter().map(|c| {
        let check_name = c.name.to_string();
        format!("{runner_metric_prefix}: {check_name} continuous maintenance")
    }).collect::<Vec<_>>();
    let check_evaluation_metric_names = checks.iter().map(|c| {
        let check_name = c.name.to_string();
        format!("{runner_metric_prefix}: {check_name} evaluation")
    }).collect::<Vec<_>>();

    let check_refresh_timing_metric_records = checks
        .iter()
        .zip(check_evaluation_metric_names.iter())
        .map(|(c, metric_name)| {
            let name = &c.name;
            quote! {
                ::bevy_godot4::prelude::record_system_duration_for_current_scope(
                    #metric_name,
                    check_timing.#name,
                );
            }
        })
        .collect::<Vec<_>>();

    let report_allowance_terms = checks.iter().map(|c| {
        let name = &c.name;
        quote! { && report.#name.is_ok() }
    }).collect::<Vec<_>>();

    let continuous_check_refreshes = checks.iter().enumerate().zip(continuous_check_metric_names.iter()).map(|((index, c), metric_name)| {
        let p_method = format_ident!("p{}", index);
        let constant = format_ident!("CHECK_MASK_{}", c.name.to_string().to_upper_camel_case());
        quote! {
            let continuous_check_started_at = ::std::time::Instant::now();
            {
                let mut check_subsystem = action.checks.subsystems.#p_method();
                if let Some(continuous_check) = ::bevy_godot4::action_framework::Check::continuous_check(&mut check_subsystem) {
                    continuous_check_mask |= #constant;
                    for action_change in action_change_list.iter() {
                        let partial_params = (!action_change.is_removed)
                            .then(|| action.manager.get_entry(&action_change.action_instance_id))
                            .flatten()
                            .map(|entry| &entry.params as &dyn ::std::any::Any);
                        continuous_check.apply_action_change(
                            action_change.action_instance_id,
                            partial_params,
                        );
                    }
                    if static_data_change_pending {
                        continuous_check.apply_static_data_change(&action.manager.static_data);
                    }
                    let dirty_action_id_list = continuous_check.flush_dirty_action_id_list();
                    action
                        .manager
                        .mark_check_mask_dirty(&dirty_action_id_list, #constant);
                }
            }
            ::bevy_godot4::prelude::record_system_duration_for_current_scope(
                #metric_name,
                continuous_check_started_at.elapsed(),
            );
        }
    }).collect::<Vec<_>>();

    let status_field_defs = checks.iter().map(|c| {
        let name = &c.name;
        quote! { pub #name: i64, }
    });

    let status_field_defaults = checks.iter().map(|c| {
        let name = &c.name;
        quote! { status_mut.#name = ::bevy_godot4::action_framework::ActionCheckStatus::NotProvided.as_i64(); }
    });

    let report_dto_payload_defs = checks.iter().zip(check_aliases.iter()).map(|(c, alias)| {
        let name = &c.name;
        quote! {
            pub #name: Option<Gd<<#alias as DataTransferConfig>::DtoType>>,
        }
    });

    let report_dto_payload_defaults = checks.iter().map(|c| {
        let name = &c.name;
        quote! { dto_mut.#name = None; }
    });

    let transfer_update_fields = checks.iter().zip(check_aliases.iter()).map(|(c, alias)| {
        let name = &c.name;
        quote! {
            let (status_code, payload_dto) = match &data.#name {
                ::bevy_godot4::action_framework::Criterion::Ok { payload } => {
                    (
                        ::bevy_godot4::action_framework::ActionCheckStatus::Ok.as_i64(),
                        payload
                            .as_ref()
                            .map(|p| <#alias as DataTransferConfig>::from_data(p, identity)),
                    )
                }
                ::bevy_godot4::action_framework::Criterion::NotProvided { payload } => {
                    (
                        ::bevy_godot4::action_framework::ActionCheckStatus::NotProvided.as_i64(),
                        payload
                            .as_ref()
                            .map(|p| <#alias as DataTransferConfig>::from_data(p, identity)),
                    )
                }
                ::bevy_godot4::action_framework::Criterion::Fail { payload, .. } => {
                    (
                        ::bevy_godot4::action_framework::ActionCheckStatus::Failure.as_i64(),
                        payload
                            .as_ref()
                            .map(|p| <#alias as DataTransferConfig>::from_data(p, identity)),
                    )
                }
            };

            {
                let mut dto_mut = dto.bind_mut();
                dto_mut.#name = payload_dto;
                let mut status = dto_mut.status_codes.bind_mut();
                status.#name = status_code;
            }
        }
    });

    quote! {
        mod #module_name {
            use super::*;
            use ::bevy::prelude::*;
            use ::bevy::ecs::system::SystemParam;
            use ::bevy_godot4::prelude::DataTransferConfig;
            use ::bevy_godot4::prelude::BevyApp;
            use ::bevy_godot4::prelude::{InitializationCoordinator, InitializationPhase};
            use ::godot::classes::{Node, RefCounted};
            use ::godot::obj::{Base, Gd};
            use ::godot::prelude::*;
            use ::std::collections::{HashMap, VecDeque};

            pub type ActionInstanceId = ::bevy_godot4::action_framework::ActionInstanceId;
            pub type ExecutionId = ::bevy_godot4::action_framework::ExecutionId;
            pub type CheckReason = ::bevy_godot4::action_framework::CheckReason;
            type FullParams = <#partial_params as ::bevy_godot4::action_framework::ActionParams>::FullParams;
            type ExecutePayload = #execute_result_payload;
            type ExecuteResultData = ::bevy_godot4::action_framework::ExecuteResult<ExecutePayload>;
            type StaticData = <#execute_subsystem<'static, 'static> as ::bevy_godot4::action_framework::ExecuteAction<
                FullParams,
                ExecuteResultData,
            >>::StaticData;
            #( #check_type_alias_defs )*
            #( #check_subsystem_type_alias_defs )*
            type ReportData = #check_report_name;
            type ReportDto = #check_report_dto_name;
            type PartialParamsDto = <#partial_params as DataTransferConfig>::DtoType;
            type ExecutePayloadDto = <ExecutePayload as DataTransferConfig>::DtoType;
            type StaticDataDto = <StaticData as DataTransferConfig>::DtoType;

            #[derive(Debug, Clone, PartialEq)]
            pub struct #check_report_name {
                pub allowance: ::bevy_godot4::action_framework::AllowanceSummary,
                #( #check_field_defs )*
            }

            impl #check_report_name {
                pub fn new(
                    #( #check_field_args ),*
                ) -> Self {
                    let allowance = if true #( #check_allowance_terms )* {
                        ::bevy_godot4::action_framework::AllowanceSummary::Ok
                    } else {
                        ::bevy_godot4::action_framework::AllowanceSummary::NotAllowed
                    };

                    Self {
                        allowance,
                        #( #check_field_inits )*
                    }
                }
            }

            #( #check_mask_constants )*
            const ALL_CHECK_MASK: u64 = #all_check_mask;

            impl Default for #check_report_name {
                fn default() -> Self {
                    Self {
                        allowance: ::bevy_godot4::action_framework::AllowanceSummary::NotAllowed,
                        #( #check_default_inits )*
                    }
                }
            }

            impl ::bevy_godot4::action_framework::CheckReportLike for #check_report_name {
                fn allowance(&self) -> ::bevy_godot4::action_framework::AllowanceSummary {
                    self.allowance
                }
            }

            /// A Rust-registered action changed between allowed and blocked.
            #[derive(::bevy::prelude::Message, Debug, Clone)]
            pub struct #check_change_name {
                pub action_instance_id: ActionInstanceId,
                pub report: #check_report_name,
            }

            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum ActionInstanceOrigin {
                Godot,
                Rust,
            }

            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub enum CheckReportDelivery {
                Godot,
                Rust,
            }

            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #status_codes_dto_name {
                #[base]
                base: Base<RefCounted>,
                #( #[var] #status_field_defs )*
            }

            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #check_report_dto_name {
                #[base]
                base: Base<RefCounted>,
                #[var]
                pub status_codes: Gd<#status_codes_dto_name>,
                #( #[var] #report_dto_payload_defs )*
            }

            pub struct #report_transfer_config_name;

            impl DataTransferConfig for #report_transfer_config_name {
                type DataType = #check_report_name;
                type DtoType = #check_report_dto_name;
                const USES_DEFAULT_OUTBOUND_DTO_BUILD: bool = false;

                fn update_dto(
                    dto: &mut Gd<Self::DtoType>,
                    data: &Self::DataType,
                    identity: &mut ::bevy_godot4::prelude::IdentitySubsystem,
                ) {
                    #( #transfer_update_fields )*
                }

                fn update_data(
                    _dto: &Gd<Self::DtoType>,
                    _data: &mut Self::DataType,
                    _identity: &mut ::bevy_godot4::prelude::IdentitySubsystem,
                ) {
                }

                fn from_data(
                    data: &Self::DataType,
                    identity: &mut ::bevy_godot4::prelude::IdentitySubsystem,
                ) -> Gd<Self::DtoType> {
                    let mut dto = #check_report_dto_name::new_gd();
                    {
                        let mut dto_mut = dto.bind_mut();
                        dto_mut.status_codes = {
                            let mut status = #status_codes_dto_name::new_gd();
                            {
                                let mut status_mut = status.bind_mut();
                                #( #status_field_defaults )*
                            }
                            status
                        };
                        #( #report_dto_payload_defaults )*
                    }
                    Self::update_dto(&mut dto, data, identity);
                    dto
                }
            }

            #[derive(Clone)]
            pub enum ApiRequestKind {
                UpdateParams(Gd<PartialParamsDto>),
                Execute,
                SetStaticData(Gd<StaticDataDto>),
                Done,
            }

            pub struct ApiRequest {
                pub action_instance_id: Option<ActionInstanceId>,
                pub kind: ApiRequestKind,
            }

            pub enum ApiResponse {
                Check(
                    ActionInstanceId,
                    Gd<ReportDto>,
                    Gd<ExecutePayloadDto>,
                    CheckReason,
                ),
                ExecuteEnqueued {
                    action_instance_id: ActionInstanceId,
                    execution_id: ExecutionId,
                },
                ExecuteDone {
                    execution_id: ExecutionId,
                    result: ExecuteResultData,
                    report: Gd<ReportDto>,
                    payload: Gd<ExecutePayloadDto>,
                },
                DoneAck {
                    action_instance_id: ActionInstanceId,
                },
            }

            pub enum OutResponse {
                Check(ActionInstanceId, ReportData, CheckReason),
                ExecuteEnqueued {
                    action_instance_id: ActionInstanceId,
                    execution_id: ExecutionId,
                },
                ExecuteDone {
                    execution_id: ExecutionId,
                    result: ExecuteResultData,
                    report: ReportData,
                },
                DoneAck {
                    action_instance_id: ActionInstanceId,
                },
            }

            fn _assert_generated_bounds() {
                fn _partial_params<T>()
                where
                    T: ::bevy_godot4::action_framework::ActionParams
                        + DataTransferConfig<DataType = T>
                        + Clone
                        + Default,
                {
                }

                fn _execute_payload<T>()
                where
                    T: DataTransferConfig<DataType = T> + Default,
                {
                }

                fn _static_data<T>()
                where
                    T: DataTransferConfig<DataType = T>,
                {
                }

                _partial_params::<#partial_params>();
                _execute_payload::<ExecutePayload>();
                _static_data::<StaticData>();
            }

            const _: fn() = _assert_generated_bounds;

            #[derive(Default)]
            struct ApiRequestQueue {
                q: VecDeque<ApiRequest>,
            }

            #[derive(Default)]
            struct ApiResponseQueue {
                q: VecDeque<ApiResponse>,
            }

            #[derive(Resource, Default, Debug)]
            struct ActionInstanceSeq {
                next: ActionInstanceId,
            }

            impl ActionInstanceSeq {
                fn next(&mut self) -> ActionInstanceId {
                    self.next = self.next.wrapping_add(1);
                    self.next
                }
            }

            #[derive(SystemParam)]
            struct ActionIoSubsystem<'w, 's> {
                requests: NonSendMut<'w, ApiRequestQueue>,
                responses: NonSendMut<'w, ApiResponseQueue>,
                seq: ResMut<'w, ActionInstanceSeq>,
                phantom: ::std::marker::PhantomData<&'s ()>,
            }

            impl ActionIoSubsystem<'_, '_> {
                fn extend_requests(&mut self, reqs: Vec<ApiRequest>) {
                    self.requests.q.extend(reqs);
                }

                fn drain_requests(&mut self) -> Vec<ApiRequest> {
                    self.requests.q.drain(..).collect()
                }

                fn push_response(&mut self, resp: ApiResponse) {
                    self.responses.q.push_back(resp);
                }

                fn drain_responses(&mut self) -> Vec<ApiResponse> {
                    self.responses.q.drain(..).collect()
                }

                fn next(&mut self) -> ActionInstanceId {
                    self.seq.next()
                }
            }

            fn enqueue_requests_for_app(app: &mut Gd<BevyApp>, reqs: Vec<ApiRequest>) {
                app.bind_mut().with_world_mut(|world: &mut World| {
                    let mut state: ::bevy::ecs::system::SystemState<ActionIoSubsystem<'_, '_>> =
                        ::bevy::ecs::system::SystemState::new(world);
                    {
                        let mut io = state.get_mut(world);
                        io.extend_requests(reqs);
                    }
                    state.apply(world);
                });
            }

            fn drain_responses_for_app(app: &mut Gd<BevyApp>) -> Vec<ApiResponse> {
                let mut drained: Vec<ApiResponse> = Vec::new();
                app.bind_mut().with_world_mut(|world: &mut World| {
                    let mut state: ::bevy::ecs::system::SystemState<ActionIoSubsystem<'_, '_>> =
                        ::bevy::ecs::system::SystemState::new(world);
                    drained = {
                        let mut io = state.get_mut(world);
                        io.drain_responses()
                    };
                    state.apply(world);
                });
                drained
            }

            fn next_action_instance_id_for_app(app: &mut Gd<BevyApp>) -> ActionInstanceId {
                let mut id: ActionInstanceId = 0;
                app.bind_mut().with_world_mut(|world: &mut World| {
                    let mut state: ::bevy::ecs::system::SystemState<ActionIoSubsystem<'_, '_>> =
                        ::bevy::ecs::system::SystemState::new(world);
                    id = {
                        let mut io = state.get_mut(world);
                        io.next()
                    };
                    state.apply(world);
                });
                id
            }

            #[derive(Debug, Clone)]
            pub struct Entry {
                pub params: #partial_params,
                dirty_check_mask: u64,
                is_report_refresh_pending: bool,
                report: #check_report_name,
                origin: ActionInstanceOrigin,
                last_godot_report: Option<#check_report_name>,
                last_rust_allowance: Option<::bevy_godot4::action_framework::AllowanceSummary>,
            }

            impl Entry {
                fn record_check_update(&mut self) -> Option<CheckReportDelivery> {
                    match self.origin {
                        ActionInstanceOrigin::Godot => {
                            if self.last_godot_report.as_ref() == Some(&self.report) {
                                return None;
                            }
                            self.last_godot_report = Some(self.report.clone());
                            Some(CheckReportDelivery::Godot)
                        }
                        ActionInstanceOrigin::Rust => {
                            if self.last_rust_allowance == Some(self.report.allowance) {
                                return None;
                            }
                            self.last_rust_allowance = Some(self.report.allowance);
                            Some(CheckReportDelivery::Rust)
                        }
                    }
                }
            }

            #[derive(Debug)]
            struct ContinuousActionChange {
                action_instance_id: ActionInstanceId,
                is_removed: bool,
            }

            #[derive(Resource, Default)]
            pub struct Manager {
                pub static_data: StaticData,
                static_data_change_pending: bool,
                next_execution_id: ExecutionId,
                entries: HashMap<ActionInstanceId, Entry>,
                dirty_action_id_list: ::bevy_godot4::action_framework::DirtyActionIdList,
                continuous_action_change_list: Vec<ContinuousActionChange>,
                pending_exec: VecDeque<(ExecutionId, ActionInstanceId)>,
                out_results: VecDeque<OutResponse>,
            }

            impl Manager {
                #[inline]
                fn alloc_execution_id(&mut self) -> ExecutionId {
                    let next = self.next_execution_id.wrapping_add(1);
                    self.next_execution_id = next;
                    next
                }

                pub fn update_params(&mut self, action_instance_id: ActionInstanceId, incoming: #partial_params) {
                    {
                        let entry = self.entries.entry(action_instance_id).or_insert(Entry {
                        params: #partial_params::default(),
                        dirty_check_mask: ALL_CHECK_MASK,
                        is_report_refresh_pending: true,
                        report: #check_report_name::default(),
                        origin: ActionInstanceOrigin::Godot,
                        last_godot_report: None,
                        last_rust_allowance: None,
                        });
                        ::bevy_godot4::action_framework::ActionParams::merge_from(&mut entry.params, &incoming);
                    }
                    self.continuous_action_change_list.push(ContinuousActionChange {
                        action_instance_id,
                        is_removed: false,
                    });
                    self.mark_dirty(action_instance_id);
                }

                pub fn register_rust_instance(
                    &mut self,
                    action_instance_id: ActionInstanceId,
                    params: FullParams,
                ) {
                    let partial_params = ::bevy_godot4::action_framework::ActionParams::from_full(&params);
                    self.entries.insert(
                        action_instance_id,
                        Entry {
                            params: partial_params,
                            dirty_check_mask: ALL_CHECK_MASK,
                            is_report_refresh_pending: true,
                            report: #check_report_name::default(),
                            origin: ActionInstanceOrigin::Rust,
                            last_godot_report: None,
                            last_rust_allowance: None,
                        },
                    );
                    self.continuous_action_change_list.push(ContinuousActionChange {
                        action_instance_id,
                        is_removed: false,
                    });
                    self.mark_dirty(action_instance_id);
                }

                pub fn update_registered_params(
                    &mut self,
                    action_instance_id: ActionInstanceId,
                    incoming: #partial_params,
                ) -> bool {
                    let Some(()) = self.entries.get_mut(&action_instance_id).map(|entry| {
                        ::bevy_godot4::action_framework::ActionParams::merge_from(
                            &mut entry.params,
                            &incoming,
                        );
                    }) else {
                        return false;
                    };
                    self.continuous_action_change_list.push(ContinuousActionChange {
                        action_instance_id,
                        is_removed: false,
                    });
                    self.mark_dirty(action_instance_id);
                    true
                }

                pub fn mark_dirty(&mut self, action_instance_id: ActionInstanceId) {
                    self.mark_check_mask_dirty(&[action_instance_id], ALL_CHECK_MASK);
                }

                fn mark_check_mask_dirty(
                    &mut self,
                    action_instance_id_list: &[ActionInstanceId],
                    check_mask: u64,
                ) {
                    for action_instance_id in action_instance_id_list.iter().copied() {
                        let Some(entry) = self.entries.get_mut(&action_instance_id) else {
                            continue;
                        };
                        entry.dirty_check_mask |= check_mask;
                        entry.is_report_refresh_pending = true;
                        self.dirty_action_id_list.mark(action_instance_id);
                    }
                }

                fn mark_polling_checks_dirty(&mut self, polling_check_mask: u64) {
                    if polling_check_mask == 0 {
                        return;
                    }
                    let action_instance_id_list = self
                        .entries
                        .iter()
                        .filter_map(|(action_instance_id, entry)| {
                            (entry.origin == ActionInstanceOrigin::Rust).then_some(*action_instance_id)
                        })
                        .collect::<Vec<_>>();
                    self.mark_check_mask_dirty(&action_instance_id_list, polling_check_mask);
                }

                fn mark_godot_checks_dirty(&mut self) {
                    if ALL_CHECK_MASK == 0 {
                        return;
                    }
                    let action_instance_id_list = self
                        .entries
                        .iter()
                        .filter_map(|(action_instance_id, entry)| {
                            (entry.origin == ActionInstanceOrigin::Godot).then_some(*action_instance_id)
                        })
                        .collect::<Vec<_>>();
                    self.mark_check_mask_dirty(&action_instance_id_list, ALL_CHECK_MASK);
                }

                fn drain_continuous_action_change_list(&mut self) -> Vec<ContinuousActionChange> {
                    self.continuous_action_change_list.drain(..).collect()
                }

                fn set_static_data(&mut self, static_data: StaticData) {
                    self.static_data = static_data;
                    self.static_data_change_pending = true;
                }

                fn take_static_data_change_pending(&mut self) -> bool {
                    ::std::mem::take(&mut self.static_data_change_pending)
                }

                fn drain_dirty_check_entry_list(&mut self) -> Vec<(ActionInstanceId, u64)> {
                    let action_instance_id_list = self.dirty_action_id_list.drain();
                    action_instance_id_list
                        .into_iter()
                        .filter_map(|action_instance_id| {
                            let entry = self.entries.get_mut(&action_instance_id)?;
                            let check_mask = ::std::mem::replace(&mut entry.dirty_check_mask, 0);
                            let is_report_refresh_pending = ::std::mem::replace(
                                &mut entry.is_report_refresh_pending,
                                false,
                            );
                            is_report_refresh_pending.then_some((action_instance_id, check_mask))
                        })
                        .collect()
                }

                /// Gets shared action data and one saved action without copying either.
                fn static_data_and_entry_mut(
                    &mut self,
                    action_instance_id: ActionInstanceId,
                ) -> Option<(&StaticData, &mut Entry)> {
                    let Self {
                        static_data,
                        entries,
                        ..
                    } = self;
                    let entry = entries.get_mut(&action_instance_id)?;
                    Some((static_data, entry))
                }

                pub fn resolve_full_params(&self, action_instance_id: ActionInstanceId) -> Option<FullParams> {
                    ::bevy_godot4::action_framework::ActionParams::to_full(
                        &self.entries.get(&action_instance_id)?.params,
                    )
                }

                pub fn get_entry(&self, action_instance_id: &ActionInstanceId) -> Option<&Entry> {
                    self.entries.get(action_instance_id)
                }

                pub fn get_entry_mut(&mut self, action_instance_id: &ActionInstanceId) -> Option<&mut Entry> {
                    self.entries.get_mut(action_instance_id)
                }

                pub fn iter_entries(&self) -> impl Iterator<Item = (&ActionInstanceId, &Entry)> {
                    self.entries.iter()
                }

                pub fn enqueue_execute_for(&mut self, action_instance_id: ActionInstanceId) -> ExecutionId {
                    let id = self.alloc_execution_id();
                    self.pending_exec.push_back((id, action_instance_id));
                    id
                }

                pub fn pop_next_exec(&mut self) -> Option<(ExecutionId, ActionInstanceId)> {
                    self.pending_exec.pop_front()
                }

                pub fn push_out(&mut self, r: OutResponse) {
                    self.out_results.push_back(r);
                }

                pub fn drain_out(&mut self) -> Vec<OutResponse> {
                    self.out_results.drain(..).collect()
                }

                pub fn remove_action_instance(&mut self, action_instance_id: ActionInstanceId) -> bool {
                    let was_registered = self.entries.remove(&action_instance_id).is_some();
                    if was_registered {
                        self.continuous_action_change_list.push(ContinuousActionChange {
                            action_instance_id,
                            is_removed: true,
                        });
                    }
                    self.pending_exec.retain(|(_, req)| *req != action_instance_id);
                    was_registered
                }

                /// Records an update and returns the owning delivery path when it changed.
                pub fn record_check_update(
                    &mut self,
                    action_instance_id: ActionInstanceId,
                    report: &#check_report_name,
                ) -> Option<CheckReportDelivery> {
                    let entry = self.entries.get_mut(&action_instance_id)?;
                    entry.report = report.clone();
                    entry.record_check_update()
                }
            }

            // Keeps recurring feasibility checks independent from action execution dependencies.
            #[derive(SystemParam)]
            struct #check_subsystem_name<'w, 's> {
                subsystems: ParamSet<'w, 's, ( #( #check_paramset_types, )* )>,
            }

            #[derive(Default)]
            struct #check_refresh_timing_name {
                #( #check_refresh_timing_field_defs )*
            }

            impl<'w, 's> #check_subsystem_name<'w, 's> {
                fn check_partial(
                    &mut self,
                    pp: &#partial_params,
                    static_data: &StaticData,
                ) -> #check_report_name {
                    let mut report = #check_report_name::default();
                    self.refresh_report(pp, static_data, &mut report, ALL_CHECK_MASK, None);
                    report
                }

                fn refresh_report(
                    &mut self,
                    pp: &#partial_params,
                    static_data: &StaticData,
                    report: &mut #check_report_name,
                    check_mask: u64,
                    mut check_timing: Option<&mut #check_refresh_timing_name>,
                ) {
                    #( #check_refresh_inits )*
                    report.allowance = if true #( #report_allowance_terms )* {
                        ::bevy_godot4::action_framework::AllowanceSummary::Ok
                    } else {
                        ::bevy_godot4::action_framework::AllowanceSummary::NotAllowed
                    };
                }
            }

            // The continuous runner receives only the state needed to refresh check reports.
            #[derive(SystemParam)]
            struct #check_runner_subsystem_name<'w, 's> {
                manager: ResMut<'w, Manager>,
                checks: #check_subsystem_name<'w, 's>,
            }

            #[derive(SystemParam)]
            pub struct #subsystem_name<'w, 's> {
                pub manager: ResMut<'w, Manager>,
                sequence: ResMut<'w, ActionInstanceSeq>,
                subsystems: ParamSet<'w, 's, ( #( #action_subsystem_paramset_types, )* )>,
            }

            impl<'w, 's> #subsystem_name<'w, 's> {
                /// Registers a Rust-triggered action for continuous checking without Godot output.
                pub fn register_potential_action(
                    &mut self,
                    params: FullParams,
                ) -> ActionInstanceId {
                    let action_instance_id = self.sequence.next();
                    self.manager.register_rust_instance(action_instance_id, params);
                    action_instance_id
                }

                /// Updates a registered action's parameters before its next fixed check.
                pub fn update_potential_action_params(
                    &mut self,
                    action_instance_id: ActionInstanceId,
                    params: #partial_params,
                ) -> bool {
                    self.manager
                        .update_registered_params(action_instance_id, params)
                }

                /// Stops continuous checking for an action registered by this caller.
                pub fn unregister_potential_action(
                    &mut self,
                    action_instance_id: ActionInstanceId,
                ) -> bool {
                    self.manager.remove_action_instance(action_instance_id)
                }

                pub(crate) fn check_partial(
                    &mut self,
                    pp: &#partial_params,
                ) -> #check_report_name {
                    let static_data = &self.manager.static_data;
                    let mut checks = self.subsystems.p1();
                    checks.check_partial(pp, static_data)
                }

                pub(crate) fn check(
                    &mut self,
                    params: FullParams,
                ) -> #check_report_name {
                    let pp = ::bevy_godot4::action_framework::ActionParams::from_full(&params);
                    self.check_partial(&pp)
                }

                pub(crate) fn execute(
                    &mut self,
                    params: FullParams,
                ) -> (
                    ExecuteResultData,
                    #check_report_name,
                ) {
                    let partial = ::bevy_godot4::action_framework::ActionParams::from_full(&params);
                    let report = self.check_partial(&partial);
                    if !::bevy_godot4::action_framework::CheckReportLike::is_allowed(&report) {
                        return (
                            ExecuteResultData {
                                ok: false,
                                payload: None,
                            },
                            report,
                        );
                    }

                    let static_data = &self.manager.static_data;
                    let result = {
                        let mut execution_subsystem = self.subsystems.p0();
                        <#execute_subsystem<'_, '_> as ::bevy_godot4::action_framework::ExecuteAction<
                            FullParams,
                            ExecuteResultData,
                        >>::execute_action(&mut execution_subsystem, static_data, params)
                    };

                    (result, report)
                }
            }

            #[derive(SystemParam)]
            pub struct #check_changes_subsystem_name<'w, 's> {
                check_changes: MessageReader<'w, 's, #check_change_name>,
            }

            impl #check_changes_subsystem_name<'_, '_> {
                /// Returns each initial or changed report published since this system last read it.
                pub fn flush_active_changes(&mut self) -> Vec<#check_change_name> {
                    self.check_changes.read().cloned().collect()
                }
            }

            pub fn api_drain_system<'w, 's>(
                mut manager: ResMut<Manager>,
                mut io: ActionIoSubsystem,
                mut identity: ::bevy_godot4::prelude::IdentitySubsystem<'w, 's>,
            ) {
                for req in io.drain_requests() {
                    match req.kind {
                        ApiRequestKind::UpdateParams(params) => {
                            let Some(action_instance_id) = req.action_instance_id else {
                                warn!("[{}] UpdateParams without action_instance_id; ignoring", #action_name);
                                continue;
                            };
                            let params = <#partial_params as DataTransferConfig>::from_dto(
                                &params,
                                &mut identity,
                            );
                            manager.update_params(action_instance_id, params);
                            manager.mark_dirty(action_instance_id);
                        }
                        ApiRequestKind::Execute => {
                            let Some(action_instance_id) = req.action_instance_id else {
                                warn!("[{}] Execute without action_instance_id; ignoring", #action_name);
                                continue;
                            };
                            let execution_id = manager.enqueue_execute_for(action_instance_id);
                            manager.push_out(OutResponse::ExecuteEnqueued {
                                action_instance_id,
                                execution_id,
                            });
                        }
                        ApiRequestKind::SetStaticData(data) => {
                            let static_data = <StaticData as DataTransferConfig>::from_dto(
                                &data,
                                &mut identity,
                            );
                            manager.set_static_data(static_data);
                        }
                        ApiRequestKind::Done => {
                            let Some(action_instance_id) = req.action_instance_id else {
                                warn!("[{}] Done without action_instance_id; ignoring", #action_name);
                                continue;
                            };
                            manager.remove_action_instance(action_instance_id);
                            manager.push_out(OutResponse::DoneAck { action_instance_id });
                        }
                    }
                }
            }

            pub fn push_checks_runner_system<'w, 's>(
                mut action: #check_runner_subsystem_name<'w, 's>,
                mut check_changes: MessageWriter<#check_change_name>,
            ) {
                // Read action changes once before rules use them.
                let action_change_drain_started_at = ::std::time::Instant::now();
                let action_change_list = action.manager.drain_continuous_action_change_list();
                let static_data_change_pending = action.manager.take_static_data_change_pending();
                ::bevy_godot4::prelude::record_system_duration_for_current_scope(
                    #action_change_drain_metric_name,
                    action_change_drain_started_at.elapsed(),
                );

                // Let each check maintain its own source index and emit affected candidates.
                let mut continuous_check_mask = 0;
                #( #continuous_check_refreshes )*

                // Schedule only candidate fields that still require polling or player preview updates.
                let scheduling_started_at = ::std::time::Instant::now();
                // Rust candidates poll only checks without a continuous implementation.
                action
                    .manager
                    .mark_polling_checks_dirty(ALL_CHECK_MASK & !continuous_check_mask);
                // Godot-created actions retain detailed live preview updates.
                action.manager.mark_godot_checks_dirty();
                ::bevy_godot4::prelude::record_system_duration_for_current_scope(
                    #scheduling_metric_name,
                    scheduling_started_at.elapsed(),
                );

                // Drain only IDs and masks so cached parameters and reports remain in place.
                let preparation_started_at = ::std::time::Instant::now();
                let dirty_check_entry_list = action.manager.drain_dirty_check_entry_list();
                ::bevy_godot4::prelude::record_system_duration_for_current_scope(
                    #preparation_metric_name,
                    preparation_started_at.elapsed(),
                );

                let mut check_timing = #check_refresh_timing_name::default();
                let mut candidate_evaluation_duration = ::std::time::Duration::ZERO;
                let mut report_delivery_duration = ::std::time::Duration::ZERO;
                for (action_instance_id, check_mask) in dirty_check_entry_list {
                    let candidate_evaluation_started_at = ::std::time::Instant::now();
                    let delivery = {
                        let Some((static_data, entry)) = action
                            .manager
                            .static_data_and_entry_mut(action_instance_id)
                        else {
                            continue;
                        };
                        action.checks.refresh_report(
                            &entry.params,
                            static_data,
                            &mut entry.report,
                            check_mask,
                            Some(&mut check_timing),
                        );
                        entry.record_check_update()
                    };
                    candidate_evaluation_duration += candidate_evaluation_started_at.elapsed();

                    // Deliver changed reports only to the side that registered the action.
                    let report_delivery_started_at = ::std::time::Instant::now();
                    match delivery {
                        Some(CheckReportDelivery::Godot) => {
                            let Some(report) = action
                                .manager
                                .entries
                                .get(&action_instance_id)
                                .map(|entry| entry.report.clone())
                            else {
                                continue;
                            };
                            action.manager.push_out(OutResponse::Check(
                                action_instance_id,
                                report,
                                CheckReason::AsyncCheck,
                            ));
                        }
                        Some(CheckReportDelivery::Rust) => {
                            let Some(report) = action
                                .manager
                                .entries
                                .get(&action_instance_id)
                                .map(|entry| entry.report.clone())
                            else {
                                continue;
                            };
                            check_changes.write(#check_change_name {
                                action_instance_id,
                                report,
                            });
                        }
                        None => {}
                    }
                    report_delivery_duration += report_delivery_started_at.elapsed();
                }
                ::bevy_godot4::prelude::record_system_duration_for_current_scope(
                    #evaluation_metric_name,
                    candidate_evaluation_duration,
                );
                #( #check_refresh_timing_metric_records )*
                ::bevy_godot4::prelude::record_system_duration_for_current_scope(
                    #delivery_metric_name,
                    report_delivery_duration,
                );
            }

            pub fn apply_exec_system<'w, 's>(mut action: #subsystem_name<'w, 's>) {
                while let Some((execution_id, action_instance_id)) = action.manager.pop_next_exec() {
                    if let Some(params) = action.manager.resolve_full_params(action_instance_id) {
                        let (result, report) = action.execute(params);
                        action.manager.push_out(OutResponse::ExecuteDone {
                            execution_id,
                            result,
                            report,
                        });
                    } else {
                        let partial = action
                            .manager
                            .get_entry(&action_instance_id)
                            .map(|e| e.params.clone())
                            .unwrap_or_default();
                        let report = action.check_partial(&partial);
                        action.manager.push_out(OutResponse::ExecuteDone {
                            execution_id,
                            result: ExecuteResultData {
                                ok: false,
                                payload: None,
                            },
                            report,
                        });
                    }
                }
            }

            pub fn publish_out_system<'w, 's>(
                mut manager: ResMut<Manager>,
                mut io: ActionIoSubsystem,
                mut identity: ::bevy_godot4::prelude::IdentitySubsystem<'w, 's>,
            ) {
                for response in manager.drain_out() {
                    match response {
                        OutResponse::Check(action_instance_id, report, reason) => {
                            let dto = <#report_transfer_config_name as DataTransferConfig>::from_data(
                                &report,
                                &mut identity,
                            );
                            let default_payload = ExecutePayload::default();
                            let payload_dto = <ExecutePayload as DataTransferConfig>::from_data(
                                &default_payload,
                                &mut identity,
                            );
                            io.push_response(ApiResponse::Check(
                                action_instance_id,
                                dto,
                                payload_dto,
                                reason,
                            ));
                        }
                        OutResponse::ExecuteEnqueued {
                            action_instance_id,
                            execution_id,
                        } => {
                            io.push_response(ApiResponse::ExecuteEnqueued {
                                action_instance_id,
                                execution_id,
                            });
                        }
                        OutResponse::ExecuteDone {
                            execution_id,
                            result,
                            report,
                        } => {
                            let dto = <#report_transfer_config_name as DataTransferConfig>::from_data(
                                &report,
                                &mut identity,
                            );
                            let payload_dto = if let Some(payload) = result.payload.as_ref() {
                                <ExecutePayload as DataTransferConfig>::from_data(payload, &mut identity)
                            } else {
                                let default_payload = ExecutePayload::default();
                                <ExecutePayload as DataTransferConfig>::from_data(
                                    &default_payload,
                                    &mut identity,
                                )
                            };
                            io.push_response(ApiResponse::ExecuteDone {
                                execution_id,
                                result,
                                report: dto,
                                payload: payload_dto,
                            });
                        }
                        OutResponse::DoneAck { action_instance_id } => {
                            io.push_response(ApiResponse::DoneAck { action_instance_id });
                        }
                    }
                }
            }

            #[derive(GodotClass)]
            #[class(init, base=RefCounted)]
            pub struct #instance_name {
                #[base]
                base: Base<RefCounted>,
                action_instance_id: ActionInstanceId,
                pending_execution_id: Option<ExecutionId>,
                bevy_app: Option<Gd<BevyApp>>,
                partial_params_dto: Gd<PartialParamsDto>,
                push_pending: bool,
            }

            #[godot_api]
            impl #instance_name {
                pub fn new_with_instance_id(action_instance_id: ActionInstanceId, bevy_app: Gd<BevyApp>) -> Gd<Self> {
                    let mut gd = Self::new_gd();
                    {
                        let mut b = gd.bind_mut();
                        b.action_instance_id = action_instance_id;
                        b.pending_execution_id = None;
                        b.bevy_app = Some(bevy_app);
                        b.partial_params_dto = <PartialParamsDto as ::godot::obj::NewGd>::new_gd();
                        b.push_pending = false;
                    }
                    gd
                }

                #[signal]
                fn on_action_status_changed(kind: i64, report: Gd<ReportDto>, payload: Gd<ExecutePayloadDto>);

                #[func]
                fn update_params(&mut self, params: Gd<PartialParamsDto>) -> Gd<Self> {
                    self.partial_params_dto = params;
                    self.schedule_push();
                    self.to_gd()
                }

                fn schedule_push(&mut self) {
                    if !self.push_pending {
                        self.push_pending = true;
                        let callable = Callable::from_object_method(&self.to_gd(), "_flush_params_now");
                        let _ = callable.call_deferred(&[]);
                    }
                }

                #[func]
                fn _flush_params_now(&mut self) {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        godot_error!("[{}] missing BevyApp in action instance", #action_name);
                        return;
                    };

                    enqueue_requests_for_app(&mut app, vec![ApiRequest {
                        action_instance_id: Some(self.action_instance_id),
                        kind: ApiRequestKind::UpdateParams(self.partial_params_dto.clone()),
                    }]);
                    self.push_pending = false;
                }

                #[func]
                fn execute(&mut self) {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        godot_error!("[{}] missing BevyApp in action instance", #action_name);
                        return;
                    };

                    enqueue_requests_for_app(
                        &mut app,
                        vec![
                            ApiRequest {
                                action_instance_id: Some(self.action_instance_id),
                                kind: ApiRequestKind::UpdateParams(self.partial_params_dto.clone()),
                            },
                            ApiRequest {
                                action_instance_id: Some(self.action_instance_id),
                                kind: ApiRequestKind::Execute,
                            },
                        ],
                    );
                    self.push_pending = false;
                }

                #[func]
                fn done(&mut self) {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        godot_error!("[{}] missing BevyApp in action instance", #action_name);
                        return;
                    };

                    enqueue_requests_for_app(&mut app, vec![ApiRequest {
                        action_instance_id: Some(self.action_instance_id),
                        kind: ApiRequestKind::Done,
                    }]);
                }
            }

            #[derive(GodotClass)]
            #[class(init, base=Node)]
            pub struct #node_name {
                #[base]
                base: Base<Node>,
                bevy_app: Option<Gd<BevyApp>>,
                instances_by_action_instance_id: HashMap<ActionInstanceId, Gd<#instance_name>>,
                instances_by_execution_id: HashMap<ExecutionId, Gd<#instance_name>>,
                static_data_cache: Option<Gd<StaticDataDto>>,
                pending_static_data_sync: bool,
            }

            #[godot_api]
            impl INode for #node_name {
                fn enter_tree(&mut self) {
                    let host = self.base().clone().upcast::<Node>();
                    match BevyApp::resolve(&host) {
                        Ok(app) => {
                            let mut app = app;
                            self.bevy_app = Some(app.clone());
                            self.pending_static_data_sync = self.static_data_cache.is_some();

                            if self.pending_static_data_sync && app.bind().get_app().is_some() {
                                if let Some(static_data) = self.static_data_cache.as_ref().cloned() {
                                    enqueue_requests_for_app(&mut app, vec![ApiRequest {
                                        action_instance_id: None,
                                        kind: ApiRequestKind::SetStaticData(static_data),
                                    }]);
                                    self.pending_static_data_sync = false;
                                }
                            }
                        }
                        Err(err) => {
                            self.bevy_app = None;
                            godot_error!("[{}] failed to bind BevyApp: {}", #action_name, err);
                        }
                    }
                }

                fn ready(&mut self) {
                    InitializationCoordinator::register_initializer_node(
                        self.base().clone().upcast(),
                        InitializationPhase::Configuration,
                    );
                }

                fn process(&mut self, _delta: f64) {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        return;
                    };

                    if self.pending_static_data_sync && app.bind().get_app().is_some() {
                        if let Some(static_data) = self.static_data_cache.as_ref().cloned() {
                            enqueue_requests_for_app(&mut app, vec![ApiRequest {
                                action_instance_id: None,
                                kind: ApiRequestKind::SetStaticData(static_data),
                            }]);
                        }
                        self.pending_static_data_sync = false;
                    }

                    for response in drain_responses_for_app(&mut app) {
                        match response {
                            ApiResponse::Check(action_instance_id, report, payload, reason) => {
                                if let Some(instance) = self.instances_by_action_instance_id.get(&action_instance_id) {
                                    let _ = reason;
                                    instance
                                        .signals()
                                        .on_action_status_changed()
                                        .emit(
                                            ::bevy_godot4::action_framework::ActionStatus::Checked.as_i64(),
                                            &report,
                                            &payload,
                                        );
                                }
                            }
                            ApiResponse::ExecuteEnqueued { action_instance_id, execution_id } => {
                                if let Some(mut instance) = self.instances_by_action_instance_id.get(&action_instance_id).cloned() {
                                    instance.bind_mut().pending_execution_id = Some(execution_id);
                                    self.instances_by_execution_id.insert(execution_id, instance);
                                }
                            }
                            ApiResponse::ExecuteDone {
                                execution_id,
                                result,
                                report,
                                payload,
                            } => {
                                if let Some(mut instance) = self.instances_by_execution_id.remove(&execution_id) {
                                    instance
                                        .signals()
                                        .on_action_status_changed()
                                        .emit(
                                            if result.ok {
                                                ::bevy_godot4::action_framework::ActionStatus::ExecutionSuccess
                                                    .as_i64()
                                            } else {
                                                ::bevy_godot4::action_framework::ActionStatus::ExecutionFailed
                                                    .as_i64()
                                            },
                                            &report,
                                            &payload,
                                        );
                                    instance.bind_mut().pending_execution_id = None;
                                }
                            }
                            ApiResponse::DoneAck { action_instance_id } => {
                                if let Some(instance) = self.instances_by_action_instance_id.remove(&action_instance_id) {
                                    if let Some(execution_id) = instance.bind().pending_execution_id {
                                        self.instances_by_execution_id.remove(&execution_id);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            #[godot_api]
            impl #node_name {
                #[func]
                fn initialize(&mut self) {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        return;
                    };

                    if self.pending_static_data_sync && app.bind().get_app().is_some() {
                        if let Some(static_data) = self.static_data_cache.as_ref().cloned() {
                            enqueue_requests_for_app(&mut app, vec![ApiRequest {
                                action_instance_id: None,
                                kind: ApiRequestKind::SetStaticData(static_data),
                            }]);
                        }
                        self.pending_static_data_sync = false;
                    }
                }

                #[func]
                fn resolve(owner: Gd<Node>) -> Option<Gd<#node_name>> {
                    if !owner.is_instance_valid() {
                        return None;
                    }

                    let Ok(app) = bevy_godot4::prelude::BevyApp::resolve(&owner) else {
                        return None;
                    };

                    let mut found = bevy_godot4::prelude::collect_children::<#node_name>(app.upcast::<Node>(), false);

                    if found.len() > 1 {
                        panic!(
                            "Multiple {} nodes found under the owner node; expected exactly one",
                            stringify!(#node_name)
                        );
                    }

                    found.pop()
                }

                #[func]
                fn start(&mut self) -> Gd<#instance_name> {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        panic!("[{}] start() called before BevyApp binding", #action_name);
                    };

                    let action_instance_id = next_action_instance_id_for_app(&mut app);
                    let instance = #instance_name::new_with_instance_id(action_instance_id, app.clone());
                    self.instances_by_action_instance_id
                        .insert(action_instance_id, instance.clone());

                    enqueue_requests_for_app(&mut app, vec![ApiRequest {
                        action_instance_id: Some(action_instance_id),
                        kind: ApiRequestKind::UpdateParams(<PartialParamsDto as ::godot::obj::NewGd>::new_gd()),
                    }]);

                    instance
                }

                #[func]
                fn get_static_data(&mut self) -> Gd<StaticDataDto> {
                    if let Some(value) = &self.static_data_cache {
                        return value.clone();
                    }

                    let value = <StaticDataDto as ::godot::obj::NewGd>::new_gd();
                    self.static_data_cache = Some(value.clone());
                    value
                }

                #[func]
                fn set_static_data(&mut self, static_data: Gd<StaticDataDto>) {
                    self.static_data_cache = Some(static_data.clone());
                    self.pending_static_data_sync = true;
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        return;
                    };

                    if app.bind().get_app().is_none() {
                        return;
                    }

                    enqueue_requests_for_app(&mut app, vec![ApiRequest {
                        action_instance_id: None,
                        kind: ApiRequestKind::SetStaticData(static_data),
                    }]);
                    self.pending_static_data_sync = false;
                }
            }

            pub(crate) struct #plugin_name;

            impl Plugin for #plugin_name {
                fn build(&self, app: &mut App) {
                    app.init_resource::<Manager>();
                    app.init_resource::<ActionInstanceSeq>();
                    app.insert_non_send_resource(ApiRequestQueue::default());
                    app.insert_non_send_resource(ApiResponseQueue::default());
                    app.add_message::<#check_change_name>();
                    app.add_systems(FixedPreUpdate, api_drain_system.in_set(#action_set))
                        .add_systems(
                            FixedUpdate,
                            (
                                push_checks_runner_system,
                                apply_exec_system,
                            )
                                .in_set(#action_set),
                        )
                        .add_systems(FixedPostUpdate, publish_out_system.in_set(#action_set));
                }
            }

            pub(crate) use #subsystem_name as ActionSubsystem;
            pub(crate) use #check_changes_subsystem_name as ActionCheckChangeSubsystem;
            pub(crate) use #plugin_name as ActionPlugin;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{expand_spec, Spec};
    use quote::quote;

    #[test]
    fn zero_check_pipeline_should_schedule_its_initial_report_refresh() {
        let spec = syn::parse2::<Spec>(quote! {
            name: ZeroCheck,
            partial_params: PartialParams,
            execute_result_payload: ExecutePayload,
            checks: [],
            execute_subsystem: ExecuteSubsystem,
            action_set: ActionSet,
        })
        .unwrap();

        let generated = expand_spec(spec).to_string();

        assert!(generated.contains("const ALL_CHECK_MASK : u64 = 0"));
        assert!(generated.contains("is_report_refresh_pending"));
        assert!(generated.contains("is_report_refresh_pending . then_some"));
    }
}
