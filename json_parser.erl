%% http://www.ietf.org/rfc/rfc4627.txt
-module(json_parser).

%% API
-export([parse/1, test/0]).

%% @doc Parse a full value (no unparsed rest is allowed)
-spec parse(JSON :: string()) -> {ok, term()}.
parse(JSON) ->
    case parse_term(strip(JSON)) of
        {ok, Type, {Term, Rest}} ->
            Rest2 = strip(Rest),
            if
                Rest2 == "" -> {ok, Term};
                true ->
                    {error, extra_data, {Type, Term, Rest2}}
            end;
        {error, Type, {State, Term, Rest}} ->
            {error, Type, {State, Term, Rest}}
    end.

%% @doc Parse a partial value (unparsed rest is allowed)
-spec parse_part(JSON :: string()) -> {ok, Type :: atom(), {Term :: term(), Rest :: string()}}.
parse_part(JSON) ->
    parse_term(strip(JSON)).


%% @doc Strip leading whitespace
-spec strip(String :: string()) -> string().
strip(S) ->
    lists:dropwhile(
      fun(C) -> lists:member(C, [9, 10, 13, 32]) end, S).


%% @doc Parse the first JSON value to an Erlang term
% Atoms
-spec parse_term(JSON :: string()) -> term().
parse_term([$t, $r, $u, $e|Rest]) ->
    {ok, bool, {true, Rest}};
parse_term([$f, $a, $l, $s, $e|Rest]) ->
    {ok, bool, {false, Rest}};
parse_term([$n, $u, $l, $l|Rest]) ->
    {ok, null, {null, Rest}};
% String
parse_term([34|Rest]) ->
    parse_string(Rest);
% Number
parse_term([N|Rest]) when ($0 =< N andalso N =< $9)
                          orelse N == $+ orelse N == $- ->
    parse_number([N|Rest]);
% List
parse_term([$[|Rest]) ->
    parse_list(first, strip(Rest), []);
% Dict
parse_term([${|Rest]) ->
    parse_dict(first, strip(Rest), dict:new());
% Catch all - error
parse_term(JSON) ->
    {error, unknown_term, {JSON}}.


%% @doc Parse a JSON string-value
-spec(parse_string(JSON :: string(), String :: list()) -> binary()).
% Quote
parse_string([$\\, 34|Rest], S) ->
    parse_string(Rest, [34|S]);
% Reverse Solidus
parse_string([$\\, $\\|Rest], S) ->
    parse_string(Rest, [$\\|S]);
% Solidus
parse_string([$\\, $/|Rest], S) ->
    parse_string(Rest, [$/|S]);
% Backspace
parse_string([$\\, $b|Rest], S) ->
    parse_string(Rest, [8|S]);
% Form feed
parse_string([$\\, $f|Rest], S) ->
    parse_string(Rest, [12|S]);
% Line feed
parse_string([$\\, $n|Rest], S) ->
    parse_string(Rest, [10|S]);
% Carriage return
parse_string([$\\, $r|Rest], S) ->
    parse_string(Rest, [13|S]);
% Tab
parse_string([$\\, $t|Rest], S) ->
    parse_string(Rest, [9|S]);
% Unicode
parse_string([$\\, $u, A, B, C, D|Rest], S) ->
    parse_string(Rest, [hex:hex_to_int([A, B, C, D])|S]);
% Qoute end
parse_string([34|Rest], S) ->
    {ok, string, {unicode:characters_to_binary(lists:reverse(S), utf8), Rest}};
% Other
parse_string([C|Rest], S) ->
    parse_string(Rest, [C|S]);
% Catch all - error
parse_string("", S) ->
    {error, unclosed_string, {string, lists:reverse(S), ""}}.
% Bootstrap
parse_string(S) ->
    parse_string(S, "").

%% @doc Parse a JSON number (e.g. 17 or -12.801e7)
parse_number(first, [$0,$.|Rest], Sgn) ->
    parse_float(frac_first, Rest, [$., $0|Sgn]);

parse_number(first, [$0|Rest], _Sgn) ->
    {ok, int, {0, Rest}};

parse_number(first, [C|Rest], Sgn) when $1 =< C andalso C =< $9 ->
    parse_number(mid, Rest, [C|Sgn]);

parse_number(mid, [C|Rest], Int) when $0 =< C andalso C =< $9 ->
    parse_number(mid, Rest, [C|Int]);

parse_number(mid, [$.|Rest], Int) ->
    parse_float(frac_first, Rest, [$.|Int]);

parse_number(mid, Rest, Int) ->
    {ok, int, {list_to_integer(lists:reverse(Int)), Rest}}.

parse_number([$+|N]) ->
    parse_number(first, N, "");

parse_number([$-|N]) ->
    parse_number(first, N, "-");

parse_number(N) ->
    parse_number(first, N, "").

%% @doc Parse a JSON number with a fractional part (e.g. -12.801e7)
-spec(parse_float(Type :: atom(), Rest :: list(), Float :: list()) -> float()).
parse_float(Type, [C|Rest], Float) when (Type == frac orelse Type == frac_first) andalso
                                        $0 =< C andalso C =< $9 ->
    parse_float(frac, Rest, [C|Float]);

parse_float(frac, [C|Rest], Float) when C == $e orelse C == $E ->
    parse_float(exp_first, Rest, [$e|Float]);

parse_float(exp_first, [C|Rest], Float) when $0 =< C andalso C =< $9 ->
    parse_float(exp, Rest, [C|Float]);

parse_float(exp, [C|Rest], Float) when $0 =< C andalso C =< $9 ->
    parse_float(exp, Rest, [C|Float]);

parse_float(Type, Rest, Float) when Type == frac orelse Type == exp ->
    {ok, float, {list_to_float(lists:reverse(Float)), Rest}};

% Catch all
parse_float(Type, Rest, Float) ->
    {error, invalid_float, {Type, lists:reverse(Float), Rest}}.


%% @doc Parse a JSON list
-spec(parse_list(State :: atom(), Rest :: list(), List :: list()) -> list()).
parse_list(_State, [$]|Rest], List) ->
    {ok, list, {lists:reverse(List), Rest}};

parse_list(mid, [$,|Rest], List) ->
    {ok, _Type, {Term, NewRest}} = parse_part(Rest),
    NewList = [Term|List],
    parse_list(mid, strip(NewRest), NewList);

parse_list(first, [C|Rest], List) when C /= $, ->
    parse_list(mid, [$,,C|Rest], List);

parse_list(State, Rest, List) ->
    {error, invalid_list, {State, lists:reverse(List), Rest}}.


%% @doc Parse a JSON dict
-spec(parse_dict(State :: atom(), Rest :: list(), Dict :: dict()) -> dict()).
parse_dict(_State, [$}|Rest], Dict) ->
    {ok, dict, {Dict, Rest}};

parse_dict(mid, [$,|Rest], Dict) ->
    {ok, string, {Key, Rest2}} = parse_part(Rest),
    [$:|Rest3] = strip(Rest2),
    {ok, _Type, {Value, Rest4}} = parse_part(Rest3),
    parse_dict(mid, strip(Rest4), dict:store(Key, Value, Dict));

parse_dict(first, [C|Rest], Dict) when C /= $, ->
    parse_dict(mid, [$,,C|Rest], Dict);

parse_dict(State, Rest, Dict) ->
    {error, invalid_dict, {State, Dict, Rest}}.



test() ->
    JSON = "{
    \"glossary\": {
        \"title\": \"example glossary\",
		\"GlossDiv\": {
            \"title\": \"S\",
			\"GlossList\": {
                \"GlossEntry\": {
                    \"ID\": \"SGML\",
					\"SortAs\": \"SGML\",
					\"GlossTerm\": \"Standard Generalized Markup Language\",
					\"Acronym\": \"SGML\",
					\"Abbrev\": \"ISO 8879:1986\",
					\"GlossDef\": {
                        \"para\": \"A meta-markup language, used to create markup languages such as DocBook.\",
						\"GlossSeeAlso\": [\"GML\", \"XML\"]
                    },
					\"GlossSee\": \"markup\"
                }
            }
        }
    }
}",
    {ok, _Dict} = parse(JSON),
    JSON2 = "{\"widget\": {
    \"debug\": \"on\",
    \"window\": {
        \"title\": \"Sample Konfabulator Widget\",
        \"name\": \"main_window\",
        \"width\": 500,
        \"height\": 500
    },
    \"image\": {
        \"src\": \"Images/Sun.png\",
        \"name\": \"sun1\",
        \"hOffset\": 250,
        \"vOffset\": 250,
        \"alignment\": \"center\"
    },
    \"text\": {
        \"data\": \"Click Here\",
        \"size\": 36,
        \"style\": \"bold\",
        \"name\": \"text1\",
        \"hOffset\": 250,
        \"vOffset\": 100,
        \"alignment\": \"center\",
        \"onMouseUp\": \"sun1.opacity = (sun1.opacity / 100) * 90;\"
    }
}}",
    {ok, _Dict2} = parse(JSON2),
    JSON3 = "{\"web-app\": {
  \"servlet\": [
    {
      \"servlet-name\": \"cofaxCDS\",
      \"servlet-class\": \"org.cofax.cds.CDSServlet\",
      \"init-param\": {
        \"configGlossary:installationAt\": \"Philadelphia, PA\",
        \"configGlossary:adminEmail\": \"ksm@pobox.com\",
        \"configGlossary:poweredBy\": \"Cofax\",
        \"configGlossary:poweredByIcon\": \"/images/cofax.gif\",
        \"configGlossary:staticPath\": \"/content/static\",
        \"templateProcessorClass\": \"org.cofax.WysiwygTemplate\",
        \"templateLoaderClass\": \"org.cofax.FilesTemplateLoader\",
        \"templatePath\": \"templates\",
        \"templateOverridePath\": \"\",
        \"defaultListTemplate\": \"listTemplate.htm\",
        \"defaultFileTemplate\": \"articleTemplate.htm\",
        \"useJSP\": false,
        \"jspListTemplate\": \"listTemplate.jsp\",
        \"jspFileTemplate\": \"articleTemplate.jsp\",
        \"cachePackageTagsTrack\": 200,
        \"cachePackageTagsStore\": 200,
        \"cachePackageTagsRefresh\": 60,
        \"cacheTemplatesTrack\": 100,
        \"cacheTemplatesStore\": 50,
        \"cacheTemplatesRefresh\": 15,
        \"cachePagesTrack\": 200,
        \"cachePagesStore\": 100,
        \"cachePagesRefresh\": 10,
        \"cachePagesDirtyRead\": 10,
        \"searchEngineListTemplate\": \"forSearchEnginesList.htm\",
        \"searchEngineFileTemplate\": \"forSearchEngines.htm\",
        \"searchEngineRobotsDb\": \"WEB-INF/robots.db\",
        \"useDataStore\": true,
        \"dataStoreClass\": \"org.cofax.SqlDataStore\",
        \"redirectionClass\": \"org.cofax.SqlRedirection\",
        \"dataStoreName\": \"cofax\",
        \"dataStoreDriver\": \"com.microsoft.jdbc.sqlserver.SQLServerDriver\",
        \"dataStoreUrl\": \"jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon\",
        \"dataStoreUser\": \"sa\",
        \"dataStorePassword\": \"dataStoreTestQuery\",
        \"dataStoreTestQuery\": \"SET NOCOUNT ON;select test='test';\",
        \"dataStoreLogFile\": \"/usr/local/tomcat/logs/datastore.log\",
        \"dataStoreInitConns\": 10,
        \"dataStoreMaxConns\": 100,
        \"dataStoreConnUsageLimit\": 100,
        \"dataStoreLogLevel\": \"debug\",
        \"maxUrlLength\": 500}},
    {
      \"servlet-name\": \"cofaxEmail\",
      \"servlet-class\": \"org.cofax.cds.EmailServlet\",
      \"init-param\": {
      \"mailHost\": \"mail1\",
      \"mailHostOverride\": \"mail2\"}},
    {
      \"servlet-name\": \"cofaxAdmin\",
      \"servlet-class\": \"org.cofax.cds.AdminServlet\"},

    {
      \"servlet-name\": \"fileServlet\",
      \"servlet-class\": \"org.cofax.cds.FileServlet\"},
    {
      \"servlet-name\": \"cofaxTools\",
      \"servlet-class\": \"org.cofax.cms.CofaxToolsServlet\",
      \"init-param\": {
        \"templatePath\": \"toolstemplates/\",
        \"log\": 1,
        \"logLocation\": \"/usr/local/tomcat/logs/CofaxTools.log\",
        \"logMaxSize\": \"\",
        \"dataLog\": 1,
        \"dataLogLocation\": \"/usr/local/tomcat/logs/dataLog.log\",
        \"dataLogMaxSize\": \"\",
        \"removePageCache\": \"/content/admin/remove?cache=pages&id=\",
        \"removeTemplateCache\": \"/content/admin/remove?cache=templates&id=\",
        \"fileTransferFolder\": \"/usr/local/tomcat/webapps/content/fileTransferFolder\",
        \"lookInContext\": 1,
        \"adminGroupID\": 4,
        \"betaServer\": true}}],
  \"servlet-mapping\": {
    \"cofaxCDS\": \"/\",
    \"cofaxEmail\": \"/cofaxutil/aemail/*\",
    \"cofaxAdmin\": \"/admin/*\",
    \"fileServlet\": \"/static/*\",
    \"cofaxTools\": \"/tools/*\"},

  \"taglib\": {
    \"taglib-uri\": \"cofax.tld\",
    \"taglib-location\": \"/WEB-INF/tlds/cofax.tld\"}}}",
    {ok, _Dict3} = parse(JSON3),
    JSON4 = "{\"menu\": {
    \"header\": \"SVG Viewer\",
    \"items\": [
        {\"id\": \"Open\"},
        {\"id\": \"OpenNew\", \"label\": \"Open New\"},
        null,
        {\"id\": \"ZoomIn\", \"label\": \"Zoom In\"},
        {\"id\": \"ZoomOut\", \"label\": \"Zoom Out\"},
        {\"id\": \"OriginalView\", \"label\": \"Original View\"},
        null,
        {\"id\": \"Quality\"},
        {\"id\": \"Pause\"},
        {\"id\": \"Mute\"},
        null,
        {\"id\": \"Find\", \"label\": \"Find...\"},
        {\"id\": \"FindAgain\", \"label\": \"Find Again\"},
        {\"id\": \"Copy\"},
        {\"id\": \"CopyAgain\", \"label\": \"Copy Again\"},
        {\"id\": \"CopySVG\", \"label\": \"Copy SVG\"},
        {\"id\": \"ViewSVG\", \"label\": \"View SVG\"},
        {\"id\": \"ViewSource\", \"label\": \"View Source\"},
        {\"id\": \"SaveAs\", \"label\": \"Save As\"},
        null,
        {\"id\": \"Help\"},
        {\"id\": \"About\", \"label\": \"About Adobe CVG Viewer...\"}
    ]
}}",
    {ok, _Dict4} = parse(JSON4),
    true.
