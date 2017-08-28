-module('riak_pb_msgcodegen_prv').

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'riak_pb_msgcodegen').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 riak_pb_msgcodegen"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "A rebar plugin"},
        {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
        undefined ->
            rebar_state:project_apps(State);
        AppInfo ->
            [AppInfo]
    end,
    [begin
        Opts = rebar_app_info:opts(AppInfo),
        SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
        IncludeDir = filename:join(rebar_app_info:dir(AppInfo), "include"),
        FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.csv\$"),

        CompileFun = fun(Source, _Opts1) ->
            ModName = filename:basename(Source, ".csv"),
            TargetName = ModName ++ ".erl",
            Hrl = ModName ++ ".hrl",
            generate(Source, filename:join(SourceDir, TargetName), filename:join(IncludeDir, Hrl))
        end,

        rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
    end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%

generate(CSV, Erl, Hrl) ->
    Tuples = load_csv(CSV),
    ModName = filename:basename(CSV, ".csv"),
    Module = generate_module(ModName, Tuples),
    HrlCode = generate_hrl(Tuples),
    Formatted = erl_prettypr:format(Module),
    ok = file:write_file(Erl, Formatted),
    ok = file:write_file(Hrl, HrlCode),
    rebar_api:info("Generated ~s~n", [Erl]).

load_csv(SourceFile) ->
    {ok, Bin} = file:read_file(SourceFile),
    csv_to_tuples(unicode:characters_to_list(Bin, latin1)).

csv_to_tuples(String) ->
    Lines = string:tokens(String, [$\r,$\n]),
    [ begin
        [Code, Message, Proto] = string:tokens(Line, ","),
        {list_to_integer(Code), Message, Proto ++ "_pb"}
    end
        || Line <- Lines].


generate_hrl(Tuples) ->
    Fun = fun({Code, Msg, _}, Str) ->
        Msg1 = change_msg(Msg),
        lists:concat([Str, "-define(CMD_",string:to_upper(Msg1),",", Code, ").\n"])
    end,
    lists:foldl(Fun, "", Tuples).




generate_module(Name, Tuples) ->
    %% TODO: Add generated doc comment at the top
    Mod = erl_syntax:attribute(erl_syntax:atom(module),
        [erl_syntax:atom(Name)]),
    ExportsList = [
        erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(1))
        || Fun <- [msg_type, msg_code, decoder_for] ],

    Exports = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list(ExportsList)]),

    Clauses = generate_msg_type(Tuples) ++
        generate_msg_code(Tuples) ++
        generate_decoder_for(Tuples),

    erl_syntax:form_list([Mod, Exports|Clauses]).

generate_decoder_for(Tuples) ->
    Spec = erl_syntax:text("-spec decoder_for(non_neg_integer()) -> module().\n"),
    Name = erl_syntax:atom(decoder_for),
    Clauses = [
        erl_syntax:clause([erl_syntax:integer(Code)],
            none,
            [erl_syntax:atom(Mod)])
        || {Code, _, Mod} <- Tuples ],
    [ Spec, erl_syntax:function(Name, Clauses) ].

generate_msg_code(Tuples) ->
    Spec = erl_syntax:text("-spec msg_code(atom()) -> non_neg_integer()."),
    Name = erl_syntax:atom(msg_code),
    Clauses = [
        erl_syntax:clause([erl_syntax:atom(Msg)], none, [erl_syntax:integer(Code)])
        || {Code, Msg, _} <- Tuples ],
    [ Spec, erl_syntax:function(Name, Clauses) ].

generate_msg_type(Tuples) ->
    Spec = erl_syntax:text("-spec msg_type(non_neg_integer()) -> atom()."),
    Name = erl_syntax:atom(msg_type),
    Clauses = [
        erl_syntax:clause([erl_syntax:integer(Code)], none, [erl_syntax:atom(Msg)])
        || {Code, Msg, _} <- Tuples ],
    CatchAll = erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(undefined)]),
    [ Spec, erl_syntax:function(Name, Clauses ++ [CatchAll]) ].

change_msg(Msg) ->
    Fun = fun(Chr, Str) ->
        case Chr >= $A andalso Chr =< $Z of
            true ->
                LowChr = char_to_lower(Chr),
                Str ++ [$_, LowChr];
            false ->
                Str ++ [Chr]
        end
    end,
    lists:foldl(Fun, "", Msg).


-spec char_to_lower(char()) -> char().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.
%%
%%-spec char_to_upper(char()) -> char().
%%char_to_upper($a) -> $A;
%%char_to_upper($b) -> $B;
%%char_to_upper($c) -> $C;
%%char_to_upper($d) -> $D;
%%char_to_upper($e) -> $E;
%%char_to_upper($f) -> $F;
%%char_to_upper($g) -> $G;
%%char_to_upper($h) -> $H;
%%char_to_upper($i) -> $I;
%%char_to_upper($j) -> $J;
%%char_to_upper($k) -> $K;
%%char_to_upper($l) -> $L;
%%char_to_upper($m) -> $M;
%%char_to_upper($n) -> $N;
%%char_to_upper($o) -> $O;
%%char_to_upper($p) -> $P;
%%char_to_upper($q) -> $Q;
%%char_to_upper($r) -> $R;
%%char_to_upper($s) -> $S;
%%char_to_upper($t) -> $T;
%%char_to_upper($u) -> $U;
%%char_to_upper($v) -> $V;
%%char_to_upper($w) -> $W;
%%char_to_upper($x) -> $X;
%%char_to_upper($y) -> $Y;
%%char_to_upper($z) -> $Z;
%%char_to_upper(Ch) -> Ch.
