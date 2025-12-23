--------------------------------------------------------
--  DDL for Package HRRC46X
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "HRRC46X" AS
  -- Site: ST11
  -- Author: Chinanwat Wiw (000553)
  -- Date updated: 2024/05/09
  -- Comment: 4448#10781 reset back
  param_msg_error           varchar2(4000 char);

  v_chken                   varchar2(10 char);
  global_v_coduser          varchar2(100 char);
  global_v_codpswd          varchar2(100 char);
  global_v_lang             varchar2(10 char) := '102';
  global_v_codempid         varchar2(100 char);
  global_v_zminlvl          number;
  global_v_zwrklvl          number;
  global_v_numlvlsalst      number;
  global_v_numlvlsalen      number;
  p_zupdsal                 varchar2(100 char);

  p_codcomp                 temploy1.codcomp%type;
  p_dtestrt                 temploy1.dteempmt%type;
  p_dteend                  temploy1.dteempmt%type;

  procedure get_index (json_str_input in clob, json_str_output out clob);
  procedure gen_index (json_str_output out clob);
END HRRC46X;



/
