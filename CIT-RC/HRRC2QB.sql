--------------------------------------------------------
--  DDL for Package HRRC2QB
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "HRRC2QB" is
  param_msg_error           varchar2(4000 char);

  v_chken                   varchar2(10 char);
  global_v_coduser          varchar2(100 char);
  global_v_codpswd          varchar2(100 char);
  global_v_codempid         varchar2(100 char);
  global_v_lang             varchar2(10 char) := '102';
  global_v_zyear            number := 0;
  global_v_lrunning         varchar2(10 char);
  global_v_zminlvl          number;
  global_v_zwrklvl          number;
  global_v_numlvlsalst      number;
  global_v_numlvlsalen      number;
  v_zupdsal                 varchar2(4 char);

  global_v_codapp           varchar2(100 char)  := 'HRRC2QB';


  procedure initial_value (json_str in clob);
  procedure check_index;
  procedure get_index (json_str_input in clob, json_str_output out clob);
  procedure gen_index (json_str_output out clob);
  procedure get_export_excel_data (json_str_input in clob,json_str_output out clob);
  procedure gen_export_excel_data (json_str_input in clob,json_str_output out clob);

end HRRC2QB;


/
