--------------------------------------------------------
--  DDL for Package HRRC15E
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "HRRC15E" as
    -- Site: ST11
    -- Author: Peerasak-(Dev) (000566)
    -- Date updated: 29-10-2024
    -- Comment: Issue 4448 #11102

    param_msg_error         varchar2(4000 char);
    global_v_coduser        varchar2(100 char);
    global_v_codempid       varchar2(100 char);
    global_v_lang           varchar2(100 char) := '102';

    param_json              json_object_t;

    p_codcomp               treqest2.codcomp%type;
    p_codpos                treqest2.codpos%type;
    p_numreqst              treqest2.numreqst%type;
    p_codbrlc               treqest2.codbrlc%type;

    p_numappl               tapplinf.numappl%type;

    p_syncon_code           treqest2.syncond%type;
    p_syncon_statement      treqest2.statement%type;
    procedure get_index(json_str_input in clob,json_str_output out clob);
    procedure get_detail(json_str_input in clob,json_str_output out clob);
    procedure save_detail(json_str_input in clob,json_str_output out clob);
    procedure send_email(json_str_input in clob,json_str_output out clob);

    -- Dev-Peerasak (000566) || 01/10/2024 || Issue#11102
    procedure replace_text_by_syncond(in_syncond in clob, in_table in varchar2, v_rtbs_from out clob, v_rtbs_syncond out clob);
    -- Dev-Peerasak (000566) || 01/10/2024 || Issue#11102
end HRRC15E;


/
