--------------------------------------------------------
--  DDL for Package Body HRRP6AB_BATCH
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE BODY "HRRP6AB_BATCH" is

  procedure gen_emp9box(p_codcomp varchar, p_dteyear number, p_dteappr date, p_codappr varchar) is
    v_codcompy    tninebox.codcompy%type := hcm_util.get_codcomp_level(p_codcomp,1);
--boy    delete <<
    v_coduser       varchar2(10) := 'TJS00001';
-->>

    v_cursor_main   number;
    v_cursor_query  number;
    v_dummy         integer;
    v_stmt          varchar2(4000);
    v_codempid      temploy1.codempid%type;
    qtyempmt        number(10);

    cursor c_tninebox is
      select codcompy,codgroup,syncond
        from tninebox a
       where codcompy   = v_codcompy
         and dteeffec   = (select max(dteeffec)
                             from tninebox b
                            where b.codcompy   = a.codcompy
                              and dteeffec    <= p_dteappr)
    order by codgroup;

  begin
    --hcm_secur.get_global_secur(v_coduser,global_v_zminlvl,global_v_zwrklvl,global_v_numlvlsalst,global_v_numlvlsalen);
    --
    delete tnineboxe
     where dteyear   = p_dteyear
       and codcompy  = v_codcompy;
    --
    for r1 in c_tninebox loop
      v_stmt := '   select codempid,qtyempmt '||
                '     from v_rp_emp '||
                '    where codcomp   like '''||p_codcomp||'%'' '||
                '      and staemp    in (''1'',''3'') '||
                '      and not exists (select codempid '||
                '                        from tnineboxe b '||
                '                       where dteyear    = '||p_dteyear ||
                '                         and codcompy   = hcm_util.get_codcomp_level(v_rp_emp.codcomp,1) '||
                '                         and b.codempid = v_rp_emp.codempid) '||
                '      and '||r1.syncond ||
	              ' order by codempid';
  --dbms_output.put_line(' , '||v_stmt);
      v_cursor_main   := dbms_sql.open_cursor;
      dbms_sql.parse(v_cursor_main,v_stmt,dbms_sql.native);
      dbms_sql.define_column(v_cursor_main,1,v_codempid,100);
      dbms_sql.define_column(v_cursor_main,2,qtyempmt);
      v_dummy := dbms_sql.execute(v_cursor_main);

      while (dbms_sql.fetch_rows(v_cursor_main) > 0) loop
        dbms_sql.column_value(v_cursor_main,1,v_codempid);
        dbms_sql.column_value(v_cursor_main,2,qtyempmt);
        --if secur_main.secur2(r2.codempid,p_coduser,global_v_zminlvl,global_v_zwrklvl,v_zupdsal) then --boy    delete <<
          begin
            insert into tnineboxe(dteyear,codcompy,codgroup,codempid,
                                  agework,codappr,dteappr,dtecreate,codcreate,dteupd,coduser)
                           values(p_dteyear,r1.codcompy,r1.codgroup,v_codempid,
                                  qtyempmt,p_codappr,p_dteappr,sysdate,v_coduser,sysdate,v_coduser);
          exception when dup_val_on_index then null;
          end;
        --end if;
      end loop;
    end loop;
    commit;
  end;
end;


/
