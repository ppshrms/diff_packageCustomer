--------------------------------------------------------
--  DDL for Package HRRP6AB_BATCH
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "HRRP6AB_BATCH" is

/*  global_v_zminlvl        number;
  global_v_zwrklvl        number;
  global_v_numlvlsalst 	  number;
  global_v_numlvlsalen 	  number;
  v_zupdsal               varchar2(20 char);
*/
  procedure gen_emp9box(p_codcomp varchar, p_dteyear number, p_dteappr date, p_codappr varchar);
end;


/
