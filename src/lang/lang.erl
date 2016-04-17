%%----------------------------------------------------
%% 文本翻译
%%----------------------------------------------------
-module(lang).
-export([get/1]).

%% 获取翻译结果
get(Text) ->
    case env:get(lang) of
        "zh_TW" -> lang_tw:get(Text);
        "en_US" -> lang_us:get(Text);
        _ -> Text
    end.
