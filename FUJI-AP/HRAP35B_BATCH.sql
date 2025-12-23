--------------------------------------------------------
--  DDL for Package HRAP35B_BATCH
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE "HRAP35B_BATCH" as
  -- Site: STD
  -- Author: Bow Sarunya (000554)
  -- Date updated: 14/08/2025
  -- Comment: 4449#12007: change query cursor c_tapbudgt

  v_chken                   varchar2(10 char);
  global_v_zminlvl	        number;
	global_v_zwrklvl	        number;
	global_v_numlvlsalst	    number;
	global_v_numlvlsalen	    number;
	global_v_zupdsal		      varchar2(4 char);


procedure  start_process (p_dteyreap   number,
                          p_numtime    number,
                          p_codcomp    varchar2,
                          p_codempid   varchar2,
                          p_codreq     varchar2,
                          p_coduser    in varchar2,
                          p_lang       in varchar2) ;

procedure  start_process_9box (p_dteyreap   number,
                                p_numtime    number,
                                p_codcomp    varchar2,
                                p_codempid   varchar2,
                                p_codreq     varchar2,
                                p_coduser    in varchar2,
                                p_lang       in varchar2) ;
procedure  update_tcmptncy (p_dteyreap   number,
                            p_numtime    number,
                            p_codempid   varchar2,
                            p_numappl    varchar2,
                            p_codreq     varchar2) ;

end HRAP35B_BATCH;

/
