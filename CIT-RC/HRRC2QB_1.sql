--------------------------------------------------------
--  DDL for Package Body HRRC2QB
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE BODY "HRRC2QB" is
  procedure initial_value(json_str in clob) is
    json_obj        json_object_t;

  begin
    v_chken             := hcm_secur.get_v_chken;
    json_obj            := json_object_t(json_str);
    global_v_coduser    := hcm_util.get_string_t(json_obj, 'p_coduser');
    global_v_codpswd    := hcm_util.get_string_t(json_obj, 'p_codpswd');
    global_v_lang       := hcm_util.get_string_t(json_obj, 'p_lang');
    global_v_codempid   := hcm_util.get_string_t(json_obj, 'p_codempid');

    hcm_secur.get_global_secur(global_v_coduser, global_v_zminlvl, global_v_zwrklvl, global_v_numlvlsalst, global_v_numlvlsalen);
  end;

  procedure check_index is
    v_error   varchar2(4000);
  begin
    null;
  end;

  procedure get_index(json_str_input in clob,json_str_output out clob) is
  begin
    initial_value(json_str_input);
    check_index;
    if param_msg_error is null then
      gen_index(json_str_output);
    end if;
    if param_msg_error is not null then
      json_str_output := get_response_message(null,param_msg_error,global_v_lang);
    end if;
  exception when others then
    param_msg_error := dbms_utility.format_error_stack||' '||dbms_utility.format_error_backtrace;
    json_str_output := get_response_message('400',param_msg_error,global_v_lang);
  end;

  procedure get_export_excel_data(json_str_input in clob,json_str_output out clob) is
  begin
    gen_export_excel_data(json_str_input,json_str_output);
  exception when others then
    param_msg_error := dbms_utility.format_error_stack||' '||dbms_utility.format_error_backtrace;
    json_str_output := get_response_message('400',param_msg_error,global_v_lang);
  end;

  procedure gen_index(json_str_output out clob) as
    obj_data            json_object_t;
    obj_row             json_object_t;

    v_rcnt              number := 0;
    v_flgdata           boolean;

    cursor c_index_report is
        select namtbl tablename, namfld namfld,decode(global_v_lang, '101', nambrowe,
                                   '102', nambrowt,
                                   '103', nambrow3,
                                   '104', nambrow4,
                                   '105', nambrow5,
                                   '') nambrow
          from treport2
         where codapp = global_v_codapp
         order by numseq;

  begin
    obj_row := json_object_t();

    for i in c_index_report loop
      v_flgdata := true;

      v_rcnt  := v_rcnt + 1;
      obj_data := json_object_t();
      obj_data.put('coderror','200');
      obj_data.put('nambrow',i.nambrow);
      obj_data.put('namfld',i.namfld);
      obj_data.put('tablename',i.tablename);

      obj_row.put(to_char(v_rcnt - 1),obj_data);
    end loop;

    if v_flgdata then
        json_str_output   := obj_row.to_clob;
    else
        param_msg_error := get_error_msg_php('HR2055',global_v_lang,'TAPPFM');
    end if;

  exception when others then
    param_msg_error := dbms_utility.format_error_stack||' '||dbms_utility.format_error_backtrace;
    json_str_output := get_response_message('400',param_msg_error,global_v_lang);
  end;

  function get_commoncode_stmt(p_ms_table varchar2) return varchar2 is
    v_stmt    varchar2(4000 char);
  begin
    if p_ms_table = 'country' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodcnty
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'currency' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodcurr
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'degree' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcoddgee
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'district' then
      v_stmt  := 'select coddist code, namdiste labe, namdistt labt, codprov code1, codpost code2, null, null, null
                  from tcoddist
                  order by coddist';
    elsif p_ms_table = 'education' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodeduc
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'institute' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodinst
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'faculty' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodmajr
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'media' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodmedi
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'nationality' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodnatn
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'occupation' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodoccu
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'province' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodprov
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'race' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodregn
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'subject' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodsubj
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'subdistrict' then
      v_stmt  := 'select codsubdist code, namsubdiste labe, namsubdistt labt, coddist code1, codprov code2, null, null, null
                  from tsubdist
                  order by codsubdist';
    elsif p_ms_table = 'relationship' then
      v_stmt  := 'select list_value code,max(decode(codlang,''101'',desc_label,'''')) labe,max(decode(codlang,''102'',desc_label,'''')) labt, null, null, numseq, codapp, null
                  from tlistval
                  where lower(codapp) = ''flgref'' and numseq <> 0 and codlang in (''101'',''102'')
                  group by numseq,codapp,list_value
                  order by numseq,codapp,list_value';
    elsif p_ms_table = 'secondpos' then
      v_stmt  := 'select codpos code, nampose labe, nampost labt, null, null, null, null, null
                  from tpostn
                  order by codpos';
    elsif p_ms_table = 'prefixname' then
      v_stmt  := 'select list_value code,max(decode(codlang,''101'',desc_label,'''')) labe,max(decode(codlang,''102'',desc_label,'''')) labt, null, null, numseq, codapp, null
                  from tlistval
                  where lower(codapp) = ''codtitle'' and numseq <> 0 and codlang in (''101'',''102'')
                  group by numseq,codapp,list_value
                  order by numseq,codapp,list_value';
    elsif p_ms_table = 'marital' then
      v_stmt  := 'select list_value code,max(decode(codlang,''101'',desc_label,'''')) labe,max(decode(codlang,''102'',desc_label,'''')) labt, null, null, numseq, codapp, null
                  from tlistval
                  where lower(codapp) = ''nammarry'' and numseq <> 0 and codlang in (''101'',''102'')
                  group by numseq,codapp,list_value
                  order by numseq,codapp,list_value';
    elsif p_ms_table = 'military' then
      v_stmt  := 'select list_value code,max(decode(codlang,''101'',desc_label,'''')) labe,max(decode(codlang,''102'',desc_label,'''')) labt, null, null, numseq, codapp, null
                  from tlistval
                  where lower(codapp) = ''nammilit'' and numseq <> 0 and codlang in (''101'',''102'')
                  group by numseq,codapp,list_value
                  order by numseq,codapp,list_value';
    elsif p_ms_table = 'religion' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodreli
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'travelupcountry' then
      v_stmt  := 'select list_value code,max(decode(codlang,''101'',desc_label,'''')) labe,max(decode(codlang,''102'',desc_label,'''')) labt, null, null, numseq, codapp, null
                  from tlistval
                  where lower(codapp) = ''flgprov'' and numseq <> 0 and codlang in (''101'',''102'')
                  group by numseq,codapp,list_value
                  order by numseq,codapp,list_value';
    elsif p_ms_table = 'traveloverseas' then
      v_stmt  := 'select list_value code,max(decode(codlang,''101'',desc_label,'''')) labe,max(decode(codlang,''102'',desc_label,'''')) labt, null, null, numseq, codapp, null
                  from tlistval
                  where lower(codapp) = ''flgoversea'' and numseq <> 0 and codlang in (''101'',''102'')
                  group by numseq,codapp,list_value
                  order by numseq,codapp,list_value';
    elsif p_ms_table = 'jobf' then
      v_stmt  := 'select codjob code, namjobe labe, namjobt labt, null, null, null, null, null
                  from tjobcode
                  order by codjob';
    elsif p_ms_table = 'location' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodloca
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'typdoc' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodtydoc
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'lang' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcodlang
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'langscore' then
      v_stmt  := 'select list_value code,max(decode(codlang,''101'',desc_label,'''')) labe,max(decode(codlang,''102'',desc_label,'''')) labt, null, null, numseq, codapp, null
                  from tlistval
                  where lower(codapp) = ''langscore'' and numseq <> 0 and codlang in (''101'',''102'')
                  group by numseq,codapp,list_value
                  order by numseq,codapp,list_value';
    elsif p_ms_table = 'stadisb' then
      v_stmt  := 'select list_value code,max(decode(codlang,''101'',desc_label,'''')) labe,max(decode(codlang,''102'',desc_label,'''')) labt, null, null, numseq, codapp, null
                  from tlistval
                  where lower(codapp) = ''stadisb'' and numseq <> 0 and codlang in (''101'',''102'')
                  group by numseq,codapp,list_value
                  order by numseq,codapp,list_value';
    elsif p_ms_table = 'typdisp' then
      v_stmt  := 'select codcodec code, descode labe, descodt labt, null, null, null, null, null
                  from tcoddisp
                  where nvl(flgact,''1'') = ''1''
                  order by codcodec';
    elsif p_ms_table = 'user' then
      v_stmt  := 'select a.coduser code, b.namfirste||'' ''||b.namlaste labe, b.namfirstt||'' ''||b.namlastt labt,
                  a.codpswd code1,
                  a.codempid code2,
                  hcm_util.get_codcomp_level(b.codcomp,1) code3, b.codcomp code4, decode(a.flgact,''1'',''O'',''C'') code5
                  from tusrprof a, temploy1 b
                  where a.codempid = b.codempid
                  order by a.coduser';
    elsif p_ms_table = 'usercomp' then
      v_stmt  := 'select coduser code, hcm_util.get_codcomp_level(codcomp,1) labe, null, null, null, null, null, null
                  from tusrcom
                  group by coduser, hcm_util.get_codcomp_level(codcomp,1)';
    elsif p_ms_table = 'compny' then
      v_stmt  := 'select codcompy code, namcome labe, namcomt labt, null, null, null, null, null
                  from tcompny
                  order by codcompy';
    elsif p_ms_table = 'center' then
      v_stmt  := 'select codcomp code, namcente labe, namcentt labt, null, null, null, null, null
                  from tcenter
                  order by codcomp';
    elsif p_ms_table = 'setpass' then
      v_stmt  := 'select to_char(dteeffec,''dd/mm/yyyy'') code, qtypassmax, qtypassmin, qtynumdigit, qtyspecail, qtyalpbup, qtyalpblow, null
                  from tsetpass
                  order by dteeffec';
    end if;
    return v_stmt;
  end;

  procedure gen_export_excel_data(json_str_input in clob,json_str_output out clob) as
    json_obj        json_object_t;
    json_obj_list   json_object_t;
    array_list      json_array_t;

    obj_data        json_object_t;
    obj_data_row    json_object_t;
    obj_row         json_object_t;
    obj_row_table   json_object_t;
    obj_table       json_object_t;

    v_rcnt          number := 0;  -- total record of all common code
    v_numseq        number := 0;  -- total record of each common code
    v_numrow        number := 0;

    v_param         json_object_t;
    v_stmt          varchar2(4000 char);
    v_namfld        varchar2(4000 char);
    v_flgdata       boolean;
    v_flgdata_user  boolean;

    v_cursor        number;
    v_dummy         integer;
    v_code          varchar2(1000 char);
    v_labe          varchar2(1000 char);
    v_labt          varchar2(1000 char);
    v_code1         varchar2(1000 char);
    v_code2         varchar2(1000 char);
    v_code3         varchar2(1000 char);
    v_code4         varchar2(1000 char);
    v_code5         varchar2(1000 char);

    p_filename      varchar2(4000 char);
    v_table         clob;

  begin
    json_obj     := json_object_t(json_str_input);
    v_param      := json_object_t(json_obj).get_object('p_data_import');
    v_numrow     := 0;

    obj_table    := json_object_t();
    if v_param is not null then
      for i in 0..v_param.get_size - 1 loop
       json_obj_list    := hcm_util.get_json_t(v_param,to_char(i));
       v_namfld         := hcm_util.get_string_t(json_obj_list,'namfld');
       obj_row          := json_object_t();

       v_flgdata        := false;
       v_flgdata_user   := false;

       v_cursor := dbms_sql.open_cursor;
       begin
        v_stmt := get_commoncode_stmt(lower(v_namfld));
        dbms_sql.parse(v_cursor,v_stmt,dbms_sql.native);
        dbms_sql.define_column(v_cursor,1,v_code,1000);
        dbms_sql.define_column(v_cursor,2,v_labe,1000);
        dbms_sql.define_column(v_cursor,3,v_labt,1000);
        dbms_sql.define_column(v_cursor,4,v_code1,1000);
        dbms_sql.define_column(v_cursor,5,v_code2,1000);
        dbms_sql.define_column(v_cursor,6,v_code3,1000);
        dbms_sql.define_column(v_cursor,7,v_code4,1000);
        dbms_sql.define_column(v_cursor,8,v_code5,1000);
        v_dummy := dbms_sql.execute(v_cursor);
        v_numseq  := 0;
        v_rcnt    := 0;

        while dbms_sql.fetch_rows(v_cursor) > 0 loop
          v_flgdata := true;
          v_code := null;  v_labe := null;  v_labt := null;  v_code1 := null;  v_code2 := null;  v_code3 := null;  v_code4 := null;  v_code5 := null;

          dbms_sql.column_value(v_cursor,1,v_code);
          dbms_sql.column_value(v_cursor,2,v_labe);
          dbms_sql.column_value(v_cursor,3,v_labt);
          dbms_sql.column_value(v_cursor,4,v_code1);
          dbms_sql.column_value(v_cursor,5,v_code2);
          dbms_sql.column_value(v_cursor,6,v_code3);
          dbms_sql.column_value(v_cursor,7,v_code4);
          dbms_sql.column_value(v_cursor,8,v_code5);

          v_rcnt := v_rcnt + 1;
          v_numseq := v_numseq + 1;
          obj_data := json_object_t();
          obj_data.put('coderror','200');
          obj_data.put('namfld',v_namfld);
          obj_data.put('numseq',v_numseq);
          obj_data.put('code',v_code);
          obj_data.put('labe',v_labe);
          obj_data.put('labt',v_labt);
          obj_data.put('code1',v_code1);
          obj_data.put('code2',v_code2);
          obj_data.put('code3',v_code3);
          obj_data.put('code4',v_code4);
          obj_data.put('code5',v_code5);

          obj_row.put(to_char(v_rcnt), obj_data);
        end loop;   -- end while dbms_sql.fetch_rows(v_cursor) > 0 loop

        if v_flgdata then
            obj_table.put(to_char(v_namfld), obj_row);
        end if; -- end if not empty obj_row

       exception when others then null;
       end;
      end loop;     -- end for i in 0..v_param.get_size - 1 loop

      obj_data_row    := json_object_t();

      obj_data_row.put('coderror','200');
      obj_data_row.put('table',obj_table);


    end if;         -- if v_param <> 'null'

    json_str_output   := obj_data_row.to_clob;

  exception when others then
    param_msg_error := dbms_utility.format_error_stack||' '||dbms_utility.format_error_backtrace;
    json_str_output := get_response_message('400',param_msg_error,global_v_lang);
  end;

end HRRC2QB;


/
