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
}

impl Parse for Spec {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut name: Option<Ident> = None;
        let mut partial_params: Option<Type> = None;
        let mut execute_result_payload: Option<Type> = None;
        let mut checks: Option<Vec<CheckSpec>> = None;
        let mut execute_subsystem: Option<Path> = None;

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
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!(
                            "Unknown key `{other}`; expected `name`, `partial_params`, `execute_result_payload`, `checks`, `execute_subsystem`"
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

        Ok(Self {
            name,
            partial_params,
            execute_result_payload,
            checks,
            execute_subsystem,
        })
    }
}

pub fn expand(input: TokenStream) -> TokenStream {
    let spec = parse_macro_input!(input as Spec);

    let action_name_ident = spec.name;
    let action_name = action_name_ident.to_string();
    let module_name = format_ident!("{}_action_pipeline", action_name.to_snake_case());
    let plugin_name = format_ident!("{}ActionPlugin", action_name_ident);
    let subsystem_name = format_ident!("{}ActionSubsystem", action_name_ident);
    let instance_name = format_ident!("{}ActionInstanceDto", action_name_ident);
    let node_name = format_ident!("{}ActionNode", action_name_ident);
    let check_report_name = format_ident!("{}ActionCheckReport", action_name_ident);
    let check_report_dto_name = format_ident!("{}ActionCheckReportDto", action_name_ident);
    let status_codes_dto_name =
        format_ident!("{}ActionCheckReportStatusCodesDto", action_name_ident);
    let report_transfer_config_name =
        format_ident!("{}ActionCheckReportTransferConfig", action_name_ident);

    let partial_params = spec.partial_params;
    let execute_result_payload = spec.execute_result_payload;
    let checks = spec.checks;
    let execute_subsystem = spec.execute_subsystem;

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

    let subsystem_paramset_types = std::iter::once(quote! { #execute_subsystem<'w, 's> })
        .chain(
            check_subsystem_aliases
                .iter()
                .map(|alias| quote! { #alias<'w, 's> }),
        )
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
        quote! { && matches!(#name, ::bevy_godot4::action_framework::Criterion::Ok) }
    });

    let check_default_inits = checks.iter().map(|c| {
        let name = &c.name;
        quote! {
            #name: ::bevy_godot4::action_framework::Criterion::NotProvided,
        }
    });

    let check_eval_inits = checks.iter().enumerate().map(|(index, c)| {
        let name = &c.name;
        let mapper = &c.mapper_type;
        let var = format_ident!("{}_check_subsystem", c.name);
        let p_method = format_ident!("p{}", index + 1);
        quote! {
            let #name = {
                let mut #var = self.subsystems.#p_method();
                <#mapper as ::bevy_godot4::action_framework::CheckAdapter>::map_and_check(
                    pp,
                    static_data,
                    &mut #var,
                )
            };
        }
    });

    let check_call_args = checks.iter().map(|c| {
        let name = &c.name;
        quote! { #name, }
    });

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
                ::bevy_godot4::action_framework::Criterion::Ok => {
                    (
                        ::bevy_godot4::action_framework::ActionCheckStatus::Ok.as_i64(),
                        None,
                    )
                }
                ::bevy_godot4::action_framework::Criterion::NotProvided => {
                    (
                        ::bevy_godot4::action_framework::ActionCheckStatus::NotProvided.as_i64(),
                        None,
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

            #[derive(Debug, Clone)]
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
                    self.seq.next = self.seq.next.wrapping_add(1);
                    self.seq.next
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
                pub dirty: bool,
            }

            #[derive(Resource, Default)]
            pub struct Manager {
                pub static_data: StaticData,
                next_execution_id: ExecutionId,
                entries: HashMap<ActionInstanceId, Entry>,
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
                    let entry = self.entries.entry(action_instance_id).or_insert(Entry {
                        params: #partial_params::default(),
                        dirty: false,
                    });
                    ::bevy_godot4::action_framework::ActionParams::merge_from(&mut entry.params, &incoming);
                }

                pub fn mark_dirty(&mut self, action_instance_id: ActionInstanceId) {
                    if let Some(e) = self.entries.get_mut(&action_instance_id) {
                        e.dirty = true;
                    }
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

                pub fn remove_action_instance(&mut self, action_instance_id: ActionInstanceId) {
                    self.entries.remove(&action_instance_id);
                    self.pending_exec.retain(|(_, req)| *req != action_instance_id);
                }
            }

            #[derive(SystemParam)]
            pub struct #subsystem_name<'w, 's> {
                pub manager: ResMut<'w, Manager>,
                subsystems: ParamSet<'w, 's, ( #( #subsystem_paramset_types, )* )>,
            }

            impl<'w, 's> #subsystem_name<'w, 's> {
                pub(crate) fn check_partial(
                    &mut self,
                    pp: &#partial_params,
                ) -> #check_report_name {
                    let static_data = &self.manager.static_data;
                    #( #check_eval_inits )*
                    #check_report_name::new(
                        #( #check_call_args )*
                    )
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
                            manager.static_data = <StaticData as DataTransferConfig>::from_dto(
                                &data,
                                &mut identity,
                            );
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

            pub fn push_checks_runner_system<'w, 's>(mut action: #subsystem_name<'w, 's>) {
                let ids: Vec<ActionInstanceId> = action.manager.iter_entries().map(|(id, _)| *id).collect();

                for action_instance_id in ids {
                    let (params_opt, reason) = {
                        if let Some(entry) = action.manager.get_entry_mut(&action_instance_id) {
                            let params = entry.params.clone();
                            let reason = if entry.dirty {
                                entry.dirty = false;
                                CheckReason::UpdatedParamsCheck
                            } else {
                                CheckReason::AsyncCheck
                            };
                            (Some(params), reason)
                        } else {
                            (None, CheckReason::AsyncCheck)
                        }
                    };

                    if let Some(params) = params_opt {
                        let report = action.check_partial(&params);
                        action
                            .manager
                            .push_out(OutResponse::Check(action_instance_id, report, reason));
                    }
                }
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

            pub fn push_checks_runner_system_driver(world: &mut ::bevy::prelude::World) {
                let mut state: ::bevy::ecs::system::SystemState<#subsystem_name<'_, '_>> =
                    ::bevy::ecs::system::SystemState::new(world);

                let action = state.get_mut(world);
                push_checks_runner_system(action);
                state.apply(world);
            }

            pub fn apply_exec_system_driver(world: &mut ::bevy::prelude::World) {
                let mut state: ::bevy::ecs::system::SystemState<#subsystem_name<'_, '_>> =
                    ::bevy::ecs::system::SystemState::new(world);

                let action = state.get_mut(world);
                apply_exec_system(action);
                state.apply(world);
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
            }

            #[godot_api]
            impl INode for #node_name {
                fn ready(&mut self) {
                    let host = self.base().clone().upcast::<Node>();
                    match BevyApp::find_for(&host) {
                        Ok(app) => self.bevy_app = Some(app),
                        Err(err) => {
                            self.bevy_app = None;
                            godot_error!("[{}] failed to bind BevyApp: {}", #action_name, err);
                        }
                    }
                }

                fn process(&mut self, _delta: f64) {
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        return;
                    };

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
                fn resolve(owner: Gd<Node>) -> Option<Gd<#node_name>> {
                    if !owner.is_instance_valid() {
                        return None;
                    }

                    let mut found = bevy_godot4::prelude::collect_children::<#node_name>(owner, false);

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
                    let Some(mut app) = self.bevy_app.as_ref().cloned() else {
                        godot_error!("[{}] set_static_data() called before BevyApp binding", #action_name);
                        return;
                    };

                    enqueue_requests_for_app(&mut app, vec![ApiRequest {
                        action_instance_id: None,
                        kind: ApiRequestKind::SetStaticData(static_data),
                    }]);
                }
            }

            pub(crate) struct #plugin_name;

            impl Plugin for #plugin_name {
                fn build(&self, app: &mut App) {
                    app.init_resource::<Manager>();
                    app.init_resource::<ActionInstanceSeq>();
                    app.insert_non_send_resource(ApiRequestQueue::default());
                    app.insert_non_send_resource(ApiResponseQueue::default());
                    app.add_systems(FixedPreUpdate, api_drain_system)
                        .add_systems(FixedUpdate, push_checks_runner_system_driver)
                        .add_systems(FixedUpdate, apply_exec_system_driver)
                        .add_systems(FixedPostUpdate, publish_out_system);
                }
            }

            pub(crate) use #subsystem_name as ActionSubsystem;
            pub(crate) use #plugin_name as ActionPlugin;
        }
    }
    .into()
}
