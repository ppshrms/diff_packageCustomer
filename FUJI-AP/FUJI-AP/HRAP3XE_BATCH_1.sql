--------------------------------------------------------
--  DDL for Package Body HRAP3XE_BATCH
--------------------------------------------------------

  CREATE OR REPLACE EDITIONABLE PACKAGE BODY "HRAP3XE_BATCH" as

  procedure start_process (p_codapp  in varchar2,
                           p_coduser in varchar2,
                           p_numproc in number,
                           p_process in varchar2,
                           p_codcomp in varchar2,
                           p_dteyreap in varchar2,
                           p_param_json in clob)  is
  begin

    hrap3xe_batch.cal_process('HRAP3XE',p_coduser,v_numproc,p_codapp,p_codcomp,p_dteyreap,p_param_json)  ;

  end;

  procedure cal_process (p_codapp  in varchar2,
                         p_coduser in varchar2,
                         p_numproc in number,
                         p_process in varchar2,
                         p_codcomp in varchar2,
                         p_dteyreap in varchar2,
                         p_param_json in clob)  is

        v_param_json            json_object_t;
        p_sumrec                number:=0;
        p_sumerr                number:=0;
        param_json_row          json_object_t;
        v_grade                 varchar2(2 char);
        v_pctpoend              number;
        v_pctpostr              number;
        v_size                  number;
        v_pctwkstr              number;
        v_pctwkend              number;
        v_sqlerrm               varchar2(1000 char) ;
        v_runproc               number := 0;

  begin
      v_numproc  := p_numproc;
      v_process  := p_process ;

        if p_process = 'HRAP3XE' then
            delete tappraism  where dteyreap = p_dteyreap and codcomp  like p_codcomp||'%';
        else
            delete tapprais  where dteyreap = p_dteyreap and codcomp  like p_codcomp||'%';
        end if;
      commit;

      b_index_sumrec := 0;
      b_index_sumerr := 0;

    --TSTAPSALD

    process_grade(p_codcomp,p_dteyreap,p_coduser) ;
    process_salary(p_process,p_codcomp,p_dteyreap,p_coduser);
    /*if p_process  = 'HRAP3XE' then
        v_param_json := json_object_t(p_param_json);
        hcm_secur.get_global_secur(p_coduser,global_v_zminlvl,global_v_zwrklvl,v_numlvlsalst,v_numlvlsalen);
        for j in 0..v_param_json.get_size-1 loop
            param_json_row  := hcm_util.get_json_t(v_param_json,to_char(j));
            v_grade         := hcm_util.get_string_t(param_json_row,'grade');
            v_pctpostr      := to_number(hcm_util.get_string_t(param_json_row,'percst'));
            v_pctpoend      := to_number(hcm_util.get_string_t(param_json_row,'percen'));
            v_runproc       := v_runproc + 1;


            process_salary (p_codapp,p_coduser,v_runproc,p_process,p_codcomp ,p_dteyreap,v_grade,v_pctpostr,v_pctpoend);

        end loop;
    else --p_process  = 'HRAP39B' then
        for i in c_tstdis loop
            process_salary (p_codapp,p_coduser,p_numproc,p_process,p_codcomp ,p_dteyreap,i.grade,i.pctpostr,i.pctpoend);
        end loop;
    end if;

      update tprocount
       set qtyproc  = b_index_sumrec,
           qtyerr   = b_index_sumerr,
           flgproc  = 'Y'
      where codapp  = p_codapp
        and coduser = p_coduser
        and numproc = p_numproc ;

      commit;
      p_sumrec := b_index_sumrec;
      p_sumerr := b_index_sumerr;
  exception when others then
      v_sqlerrm := sqlerrm ;
    update tprocount
       set qtyproc  = b_index_sumrec,
           qtyerr   = b_index_sumerr,
           codempid = tappemp_codempid,
           dteupd   = sysdate,
           Flgproc  = 'E',
           remark   = substr('Error Step :'||v_sqlerrm,1,500)
     where codapp  = p_codapp
       and coduser = p_coduser
       and numproc = p_numproc ;*/
    commit;

    param_msg_error     := dbms_utility.format_error_stack||' '||dbms_utility.format_error_backtrace;
  end ;

    procedure process_grade ( p_codcomp  in varchar2,
                              p_dteyreap in varchar2,
                              p_coduser  in varchar) is

        v_syncond           tcontrap.syncond%type;
        v_syncondes         tcontrap.syncond%type;
        v_formusal          tapbudgt.formusal%type;
        v_formusalds        tapbudgt.formusal%type;
        v_flggrade          tapbudgt.flggrade%type;
        v_flgntover         tcontrap.flgntover%type;
        v_flgpyntover       tcontrap.flgpyntover%type;
        v_formuntover       tcontrap.formuntover%type;
        v_formuntoverd      tcontrap.formuntover%type;
        v_flgover           tcontrap.flgover%type;
        v_flgpyover         tcontrap.flgpyover%type;
        v_formuover         tcontrap.formuover%type;
        v_formuoverd        tcontrap.formuover%type;

        v_codempid          temploy1.codempid%type;
        v_dteempmt          temploy1.dteempmt%type;
        v_staemp            temploy1.staemp%type;
        v_codempmt          temploy1.codempmt%type;
        v_typemp            temploy1.typemp%type;
        v_jobgrade          temploy1.jobgrade%type;
        v_codpos            temploy1.codpos%type;
        v_typpayroll        temploy1.typpayroll%type;
        v_amtincom          temploy3.amtincom1%type;
        v_qtytotnet 	    tappemp.qtytotnet%type;
        v_qtytotnetn 	    tappemp.qtytotnet%type;
        v_timtotnet 	    number;
        v_qtyta             tappemp.qtyta%type;
        v_qtytan            tappemp.qtyta%type;
        v_timta  	        number;
        v_qtypuns           tappemp.qtypuns%type;
        v_qtypunsn          tappemp.qtypuns%type;
        v_timpuns    	    number;
        v_flgsal            tappemp.flgsal%type;
        v_pctdsal           tappemp.pctdsal%type;
        v_pctdsaln          tappemp.pctdsal%type;
        v_timpctdsal    	number;
        v_grdap             tappemp.grdap%type := '@#';

        v_grade             varchar2(2 char);
        v_flgfound          number := 0;
        v_count             number := 0;
        v_pctsal            number := 0;

        v_qtywork           number;
        v_codcomp           varchar2(40 char) ;
        v_codcomlvl         varchar2(40 char) := '@#';
        v_minqtytotnet      number;
        v_numempap          number := 0;
        v_numsyn            number := 0;
        v_empgrd            number := 0;
        v_dteyreap          number;
        v_qtykpie3          number;
        v_timkpie3          number;
        v_qtykpie3n         number;
        v_qtycmp3           number;
        v_timcmp3           number;
        v_qtycmp3n          number;
        v_qtybeh3           number;
        v_timbeh3           number;
        v_qtybeh3n          number;
        v_codaplvl          tappemp.codaplvl%type;
        v_stmt              clob;

        type char2 is table of varchar2(4 char) index by binary_integer;
            v_arrgrdap	char2;

        type numemp is table of number index by binary_integer;
            v_arrnumemp	numemp;

      cursor c_tappemp is
          select sum(nvl(qtyadjtot,nvl(qtytotnet,0))), codempid,codcomp,numlvl
            from tappemp a
           where dteyreap = p_dteyreap
             and codcomp  like p_codcomp||'%'
             and flgsal   = 'Y'
             and exists  (select codaplvl from tstdisd b
                           where a.codcomp   like b.codcomp||'%'
                             and b.dteyreap  = a.dteyreap
                             and b.numtime   = a.numtime
                             and b.flgsal    = 'Y'
                             and exists(select codaplvl
                                          from tempaplvl
                                         where dteyreap = b.dteyreap
                                           and numseq   = b.numtime
                                           and codaplvl = b.codaplvl
                                           and codempid = a.codempid )
             )
          group by codempid,codcomp,numlvl
          order by sum(nvl(qtyadjtot,nvl(qtytotnet,0))) desc,codempid;

        cursor c_tapbudgt is
            select codcomp,flggrade
              from tapbudgt
             where dteyreap   = p_dteyreap
               and codcomp    like p_codcomp||'%'
               and flggrade   is not null
            order by codcomp desc;

        cursor c_tstdis is
          select grade,pctwkstr,pctwkend,pctemp,dteyreap
            from tstdis
           where v_codcomlvl like codcomp||'%'
             and dteyreap = (select max(dteyreap)
                               from tstdis
                              where v_codcomlvl like codcomp||'%'
                                and dteyreap <= p_dteyreap)
           order by grade;

        cursor c_tstdis_grade is
          select grade,pctwkstr,pctwkend,pctemp
            from tstdis
           where v_codcomlvl like codcomp||'%'
             and grade    = nvl(v_grade,grade)
             and dteyreap = v_dteyreap
           order by grade;

        cursor c_tnineboxap is
            select syncond,codgroup
              from tnineboxap
             where codcompy = hcm_util.get_codcomp_level(v_codcomlvl,1)
               and dteeffec = (select max(dteeffec)
                                 from tnineboxap
                                where codcompy = hcm_util.get_codcomp_level(v_codcomlvl,1)
                                  and dteeffec <= trunc(sysdate));

    begin

        begin
            select syncond,flgntover,flgpyntover,formuntover,flgover,flgpyover,formuover
            into v_syncond,v_flgntover,v_flgpyntover,v_formuntover,v_flgover,v_flgpyover,v_formuover
            from tcontrap
           where codcompy  = hcm_util.get_codcomp_level(p_codcomp,1)
             and dteyreap  = (select max(dteyreap)
                                from tcontrap
                               where codcompy   = hcm_util.get_codcomp_level(p_codcomp,1)
                                 and dteyreap   <= p_dteyreap);
        exception when no_data_found then
          null;
        end;

        for i in 1..20 loop
            v_arrgrdap(i)  := null;
            v_arrnumemp(i) := null;
        end loop;

        v_numempap := 0;
        for i in c_tappemp loop
            v_numempap := v_numempap + 1;
        end loop;

        for r_tapbudgt in c_tapbudgt loop
            v_codcomp   := r_tapbudgt.codcomp;
            v_codcomlvl := r_tapbudgt.codcomp;

            for r_tstdis in c_tstdis loop
                v_numsyn := v_numsyn + 1; -- ลำดับที่ array
                v_arrgrdap(v_numsyn)  := r_tstdis.grade; -- เกรด
                v_arrnumemp(v_numsyn) := trunc((v_numempap * r_tstdis.pctemp)/100); -- จำนวนพนักงานที่จะได้ในแต่ละเกรด
                v_dteyreap := r_tstdis.dteyreap;
            end loop;

            for i in c_tappemp loop
                begin
                    select dteempmt,staemp,codempmt,typemp,jobgrade,typpayroll,codpos
                      into v_dteempmt,v_staemp,v_codempmt,v_typemp,v_jobgrade,v_typpayroll,v_codpos
                      from temploy1
                     where codempid = i.codempid;
                exception when no_data_found then
                    null;
                end;
                v_qtywork := trunc(months_between(trunc(sysdate),v_dteempmt));
                v_flgfound := 0;
                if v_syncond is not null then
                    v_syncondes := v_syncond ;
                    v_syncondes := replace(v_syncondes,'V_HRPMA1.AGE_POS',''||v_qtywork||'') ;
                    v_syncondes := replace(v_syncondes,'V_HRPMA1.STAEMP',''''||v_staemp||'''') ;
                    v_syncondes := replace(v_syncondes,'V_HRPMA1.TYPEMP',''''||v_typemp||'''') ;
                    v_syncondes := replace(v_syncondes,'V_HRPMA1.CODEMPMT',''''||v_codempmt||'''') ;
                    v_syncondes := 'select count(*) from V_HRPMA1 where '||v_syncondes||' and codempid ='''||i.codempid||'''' ;
                    v_flgfound  := execute_qty(v_syncondes) ;
                end if;

                v_flgsal := 'Y';
                begin
                    select flgsal into v_flgsal
                      from tappemp
                     where codempid  = i.codempid
                       and dteyreap  = p_dteyreap
                       and nvl(flgsal,'N')    = 'N'
                       and rownum    = 1;
                exception when no_data_found then
                    v_flgsal := 'Y';
                end;

                if v_flgfound <> 0 and v_flgsal = 'Y' then
                    v_codempid := i.codempid;

                    begin
                        select sum(nvl(qtyadjtot,nvl(qtytotnet,0))),sum(decode(nvl(qtyadjtot,nvl(qtytotnet,0)),0,0,1)),
                               sum(nvl(qtyta,0)),sum(decode(qtyta,0,0,1)),
                               sum(nvl(qtypuns,0)),sum(decode(qtypuns,0,0,1)),
                               sum(nvl(pctdsal,0)),sum(decode(pctdsal,0,0,1)),
                               sum(nvl(qtykpie3,0)),sum(decode(qtykpie3,0,0,1)),
                               sum(nvl(qtycmp3,0)),sum(decode(qtycmp3,0,0,1)),
                               sum(nvl(qtybeh3,0)),sum(decode(qtybeh3,0,0,1))
                        into   v_qtytotnet,v_timtotnet,
                               v_qtyta,v_timta,
                               v_qtypuns,v_timpuns,
                               v_pctdsal,v_timpctdsal,
                               v_qtykpie3,v_timkpie3,
                               v_qtycmp3,v_timcmp3,
                               v_qtybeh3,v_timbeh3
                         from  tappemp a
                        where  a.codempid  = i.codempid
                          and  a.dteyreap  = p_dteyreap
                          and exists  (select 1 from tstdisd b
                                        where a.codcomp   like b.codcomp||'%'
                                          and b.dteyreap  = a.dteyreap
                                          and b.numtime   = a.numtime
                                          and b.flgsal    = 'Y'
                                          and exists(select codaplvl
                                                      from tempaplvl
                                                     where dteyreap = a.dteyreap
                                                       and numseq  = a.numtime
                                                       and codaplvl = b.codaplvl
                                                       and codempid = i.codempid )
                                          );
                    exception when others then
                        v_qtytotnet := 0; v_timtotnet   := 0;
                        v_qtyta 	:= 0; v_timta       := 0;
                        v_qtypuns 	:= 0; v_timpuns     := 0;
                    end ;

                    if nvl(v_timtotnet,0) <> 0 then
                        v_qtytotnetn  := 	round(nvl(v_qtytotnet,0) / v_timtotnet,2);
                    end if;

                    if nvl(v_timta,0) <> 0 then
                        v_qtytan  := 	round(nvl(v_qtyta,0) / v_timta,2);
                    end if;

                    if nvl(v_timpuns,0) <> 0 then
                        v_qtypunsn  := 	round(nvl(v_qtypuns,0) / v_timpuns,2);
                    end if;

                    if nvl(v_timpctdsal,0) <> 0 then
                        v_pctdsaln  := 	nvl(round(nvl(v_pctdsal,0) / v_timpctdsal,2),0);
                    end if;

                    if nvl(v_timkpie3,0) <> 0 then
                        v_qtykpie3n  := 	round(nvl(v_qtykpie3,0) / v_timkpie3,2);
                    end if;

                    if nvl(v_timcmp3,0) <> 0 then
                        v_qtycmp3n  := 	round(nvl(v_qtycmp3,0) / v_timcmp3,2);
                    end if;

                    if nvl(v_timbeh3,0) <> 0 then
                        v_qtybeh3n  := 	round(nvl(v_qtybeh3,0) / v_timbeh3,2);
                    end if;

                    --cal grade 1-กำหนดคะแนน
                    if r_tapbudgt.flggrade = '1' then
                        v_grdap := null;
                        for r_tstdis in c_tstdis loop
                            if v_qtytotnetn between r_tstdis.pctwkstr and r_tstdis.pctwkend then
                                v_grdap := r_tstdis.grade;
                                exit;
                            end if;
                        end loop;
                    elsif r_tapbudgt.flggrade = '2' then

                        v_grdap := null;
                        if v_numsyn <> 0 then
                            for j in 1..v_numsyn loop
                                v_grade := v_arrgrdap(j);
                                for r_tstdis in c_tstdis_grade loop
                                    if v_arrgrdap(j) = r_tstdis.grade then
                                        if v_process = 'HRAP3XE' then
                                            v_empgrd := 0;
                                            begin
                                                select count(codempid) into v_empgrd--จำนวนพนักงานทั้งหมดที่ได้เกรดนี้ไปแล้ว
                                                  from tappraism
                                                 where dteyreap = p_dteyreap
                                                   and codcomp  like v_codcomp||'%'
                                                   and grade    = r_tstdis.grade;
                                            exception when no_data_found then
                                                v_empgrd := 0;
                                            end;
                                        else
                                            v_empgrd := 0;
                                            begin
                                                select count(codempid) into v_empgrd--จำนวนพนักงานทั้งหมดที่ได้เกรดนี้ไปแล้ว
                                                  from tapprais
                                                 where dteyreap = p_dteyreap
                                                   and codcomp  like v_codcomp||'%'
                                                   and grade    = r_tstdis.grade;
                                            exception when no_data_found then
                                                v_empgrd := 0;
                                            end;
                                        end if;

                                        if v_qtytotnetn >= r_tstdis.pctwkstr then
                                            if v_empgrd < v_arrnumemp(j) then
                                                v_grdap     := r_tstdis.grade;
                                                exit;
                                            elsif v_empgrd >= v_arrnumemp(j) then
                                                if v_process = 'HRAP3XE' then
                                                    begin
                                                        select min(qtyscor) into v_minqtytotnet
                                                          from tappraism
                                                         where dteyreap = p_dteyreap
                                                           and codcomp  like v_codcomp||'%'
                                                           and grade    = r_tstdis.grade;
                                                    exception when no_data_found then
                                                        v_minqtytotnet := 0;
                                                    end;
                                                else
                                                    begin
                                                        select min(qtyscore) into v_minqtytotnet
                                                          from tapprais
                                                         where dteyreap = p_dteyreap
                                                           and codcomp  like v_codcomp||'%'
                                                           and grade    = r_tstdis.grade;
                                                    exception when no_data_found then
                                                        v_minqtytotnet := 0;
                                                    end;

                                                end if;

                                                if v_minqtytotnet <> v_qtytotnetn then
                                                    v_grdap := null;
                                                else
                                                    v_grdap := r_tstdis.grade;
                                                    exit;
                                                end if;
                                            end if;
                                        end if;

                                    end if; ---if v_arrgrdap(j) = r_tstdis.grade then
                                end loop;   ---for r_tstdis in c_tstdis loop
                                if nvl(v_grdap,'@#') <> '@#' then
                                    exit;
                                end if;
                            end loop;       ---for j in 1..v_numsyn loop
                        end if;             ---if v_numsyn <> 0 then

                    elsif r_tapbudgt.flggrade = '3' then
                        begin
                            select codaplvl into v_codaplvl
                             from tappemp
                            where codempid  = i.codempid
                              and dteyreap  = p_dteyreap
                              and rownum    = 1;
                        exception when no_data_found then
                           v_codaplvl := null;
                        end;
                        for c9box in c_tnineboxap loop
                            v_syncond := c9box.syncond;
                            if v_syncond is not null then
                                v_stmt := v_syncond ;
                                v_stmt := replace(v_stmt,'TAPPEMP.CODCOMP',''''||i.codcomp||'''') ;
                                v_stmt := replace(v_stmt,'TAPPEMP.CODAPLVL',''''||v_codaplvl||'''') ;
                                v_stmt := replace(v_stmt,'TAPPEMP.QTYKPI',''||v_qtykpie3n||'') ;
                                v_stmt := replace(v_stmt,'TAPPEMP.QTYCMP',''||v_qtycmp3n||'') ;
                                v_stmt := replace(v_stmt,'TAPPEMP.QTYBEH',''||v_qtybeh3n||'') ;
                                v_stmt := 'select count(*) from TAPPEMP where '||v_stmt||' and codempid ='''||i.codempid||'''' ;
                                v_flgfound  := execute_qty(v_stmt) ;
                            end if;
                            if v_flgfound <> 0 then
                                v_grdap := c9box.codgroup;
                                exit;
                            end if;
                        end loop;
                    end if;


                    if v_process = 'HRAP3XE' then
                        begin
                            insert into tappraism (dteyreap,codempid,codcomp,
                                                  flgsal,qtypuns,qtyta,
                                                  grade,pctsal,qtyscor,
                                                  pctdsal,codcreate,coduser)
                                 values         ( p_dteyreap,i.codempid,i.codcomp,
                                                  v_flgsal,v_qtypunsn,v_qtytan,
                                                  v_grdap,null,v_qtytotnetn,
                                                  v_pctdsaln,p_coduser,p_coduser);
                        exception when dup_val_on_index then
                            update tappraism set flgsal     = v_flgsal,
                                                 grade      = v_grdap,
                                                 pctsal     = null,
                                                 qtyscor    = v_qtytotnetn,
                                                 pctdsal    = v_pctdsaln,
                                                 coduser    = p_coduser
                            where codempid = i.codempid
                              and dteyreap = p_dteyreap
                              and codcomp  = i.codcomp;
                        end ;
                    else
                        begin
                          insert into tapprais (  codempid,dteyreap,--codcomlvl,
                                                  codcomp,codpos,typpayroll,
                                                  numlvl,jobgrade,qtywork,
                                                  flgsal,qtypuns,qtyta,
                                                  grade,qtyscore,pctcalsal,
                                                  pctdsal,
                                                  staappr,codcreate,coduser)
                                 values         ( i.codempid,p_dteyreap,--i.codcomp,
                                                  i.codcomp,v_codpos,v_typpayroll,
                                                  i.numlvl,v_jobgrade,v_qtywork,
                                                  v_flgsal,v_qtypunsn,v_qtytan,
                                                  v_grdap,v_qtytotnetn,null,
                                                  v_pctdsaln,
                                                  'P',p_coduser,p_coduser);
                        exception when dup_val_on_index then
                          update tapprais  set flgsal     = v_flgsal,
                                               grade      = v_grdap,
                                               pctcalsal  = null,
                                               qtyscore   = v_qtytotnetn,
                                               pctdsal    = v_pctdsaln,
                                               coduser    = p_coduser
                           where codempid = i.codempid
                             and dteyreap = p_dteyreap;
                        end ;
                    end if;
                    commit;
                end if; --if v_flgfound <> 0 and v_flgsal = 'Y' then

            end loop; --for i in c_tappemp loop

        end loop;
    end;


    procedure process_salary (p_process  in varchar2,
                              p_codcomp  in varchar2,
                              p_dteyreap in varchar2,
                              p_coduser  in varchar2) is


        v_syncond           tcontrap.syncond%type;
        v_syncondes         tcontrap.syncond%type;
        v_formusal          tapbudgt.formusal%type;
        v_formusalds        tapbudgt.formusal%type;
        v_flggrade          tapbudgt.flggrade%type;
        v_flgntover         tcontrap.flgntover%type;
        v_flgpyntover       tcontrap.flgpyntover%type;
        v_formuntover       tcontrap.formuntover%type;
        v_formuntoverd      tcontrap.formuntover%type;
        v_flgover           tcontrap.flgover%type;
        v_flgpyover         tcontrap.flgpyover%type;
        v_formuover         tcontrap.formuover%type;
        v_formuoverd        tcontrap.formuover%type;

        v_codempid          temploy1.codempid%type;
        v_amtincom          temploy3.amtincom1%type;
        v_qtytotnet 	    tappemp.qtytotnet%type;
        v_qtytotnetn 	    tappemp.qtytotnet%type;
        v_timtotnet 	    number;
        v_qtyta             tappemp.qtyta%type;
        v_qtytan            tappemp.qtyta%type;
        v_timta  	        number;
        v_qtypuns           tappemp.qtypuns%type;
        v_qtypunsn          tappemp.qtypuns%type;
        v_timpuns    	    number;
        v_flgsal            tappemp.flgsal%type;
        v_pctdsal           tappemp.pctdsal%type;
        v_pctdsaln          tappemp.pctdsal%type;
        v_timpctdsal    	number;
        v_grdap             tappemp.grdap%type;
        v_grade             tappemp.grdap%type;

        v_sqlerrm           varchar2(1000 char) ;
        v_flgsecu	        boolean;
        v_zupdsal           varchar2(1  char);
        v_flgfound          number := 0;
        v_count             number := 0;
        v_pctsal            number := 0;

        v_amtminsa          number;
        v_amtmaxsa          number;
        v_midpoint          number;
        v_pctincrease       number;
        v_amtcal            number;
        v_amtcaln           number;
        v_amtdsal           number;
        v_amtsaln           number;
        v_amtover           number;
        v_amtpayover        number;
        v_qtywork           number;
        v_pctcal            number;

        v_midpointe         tappraism.amtmidsal%type;
        v_amtsaloe          tappraism.amtmidsal%type;
        v_amtcalne          tappraism.amtmidsal%type;
        v_amtsalne          tappraism.amtmidsal%type;
        v_amtmaxsae         tappraism.amtmidsal%type;
        v_amtminsae         tappraism.amtmidsal%type;
        v_amtovere          tappraism.amtmidsal%type;
        v_amtpayovere       tappraism.amtmidsal%type;
        v_amtsalnovr        number;
        v_amtcalnovr        number;
        v_dteapend          tstdisd.dteapend%type;

        v_pctpoend          number;
        v_pctpostr          number;
        v_size              number;
        v_pctwkstr          number;
        v_pctwkend          number;
        v_codcomp           varchar2(40 char) ;
        v_codcomlvl         varchar2(40 char) := '@#';
        v_unitcal1          tcontpmd.unitcal1%type;

        type char2 is table of varchar2(4 char) index by binary_integer;
            v_arrgrdap	char2;

        type numemp is table of number index by binary_integer;
            v_arrnumemp	numemp;

      cursor c_emp is
        select process,codempid,codcomp,codpos,numlvl,jobgrade,
               flgsal,qtyscor,grade,typpayroll,pctdsal,
               amtincom,dteempmt,staemp,typemp,codempmt
        from(   select 'HRAP3XE' as process, a.codempid,d.codcomp,d.codpos,d.numlvl,d.jobgrade,
                       a.flgsal,a.qtyscor,a.grade,d.typpayroll,pctdsal,
                       stddec(amtincom1,a.codempid,'2017') amtincom,
                       d.dteempmt,d.staemp,d.typemp,d.codempmt
                    from tappraism a,temploy3 c, temploy1 d
                   where a.codempid = c.codempid
                     and a.codempid = d.codempid
                     and a.dteyreap = p_dteyreap
                     and a.codcomp  like p_codcomp||'%'
                     and a.flgsal   = 'Y'
            union
                select 'HRAP39B' as process, a.codempid,d.codcomp,d.codpos,d.numlvl,d.jobgrade,
                       a.flgsal,a.qtyscore qtyscor,a.grade,d.typpayroll,pctdsal,
                       stddec(amtincom1,a.codempid,'2017') amtincom,
                       d.dteempmt,d.staemp,d.typemp,d.codempmt
                    from tapprais a,temploy3 c, temploy1 d
                   where a.codempid = c.codempid
                     and a.codempid = d.codempid
                     and a.dteyreap = p_dteyreap
                     and a.codcomp  like p_codcomp||'%'
                     and a.flgsal   = 'Y')
          where process = v_process
          order by codempid;


        cursor c_tstdis is
          select grade,pctpostr,pctpoend,pctwkstr,pctwkend
            from tstdis
           where v_codcomp like codcomp||'%'
             and grade    = v_grade
             and dteyreap = (select max(dteyreap)
                               from tstdis
                              where v_codcomp like codcomp||'%'
                                and dteyreap <= p_dteyreap)
           order by grade;


        cursor c_ttmovemt is
            select stddec(amtincom1,codempid,v_chken) amtincom
              from ttmovemt
             where codempid  = v_codempid
               and dteeffec  <= v_dteapend
             order by numseq desc;

    begin

        begin
            select syncond,flgntover,flgpyntover,formuntover,flgover,flgpyover,formuover
            into v_syncond,v_flgntover,v_flgpyntover,v_formuntover,v_flgover,v_flgpyover,v_formuover
            from tcontrap
           where codcompy  = hcm_util.get_codcomp_level(p_codcomp,1)
             and dteyreap  = (select max(dteyreap)
                                from tcontrap
                               where codcompy   = hcm_util.get_codcomp_level(p_codcomp,1)
                                 and dteyreap   <= p_dteyreap);
        exception when no_data_found then
          null;
        end;

        for i in c_emp loop

            v_flgsecu := secur_main.secur1(i.codcomp,i.numlvl,p_coduser,global_v_zminlvl,global_v_zwrklvl,v_zupdsal,v_numlvlsalst,v_numlvlsalen);
            v_flgsecu := true;
            tappemp_codempid := i.codempid;
            v_codcomp   := i.codcomp;
            v_grade     := i.grade;

            if v_flgsecu then

                v_midpoint      := 0;
                v_amtincom      := 0;
                v_amtcaln       := 0;
                v_amtsalnovr    := 0;
                v_amtmaxsa      := 0;
                v_amtminsa      := 0;
                v_amtover       := 0;
                v_amtpayover    := 0;

                v_pctincrease   := 0;
                begin
                  select amtminsa,amtmaxsa,midpoint
                    into v_amtminsa,v_amtmaxsa,v_midpoint
                    from tsalstr
                   where codcompy = hcm_util.get_codcomp_level(p_codcomp,1)
                     and jobgrade = i.jobgrade
                     and dteyreap = (select max(dteyreap)
                                       from tsalstr
                                      where codcompy = hcm_util.get_codcomp_level(p_codcomp,1)
                                        and jobgrade = i.jobgrade
                                        and dteyreap <= p_dteyreap)
                     order by dteyreap desc;
                exception when no_data_found then
                  null;
                end;

                begin
                  select unitcal1
                    into v_unitcal1
                    from tcontpmd
                   where codcompy = hcm_util.get_codcomp_level(p_codcomp,1)
                     and codempmt = i.codempmt
                     and dteeffec = (select max(dteeffec)
                                       from tcontpmd
                                      where codcompy = hcm_util.get_codcomp_level(p_codcomp,1)
                                        and codempmt = i.codempmt
                                        and dteeffec <= trunc(sysdate)
                                        and rownum   = 1)
                     and rownum   = 1;
                exception when no_data_found then
                  v_unitcal1 := null;
                end;

                ---cal form formusal
                begin
                    select formusal,flggrade
                       into v_formusal,v_flggrade
                      from tapbudgt
                     where dteyreap   = p_dteyreap
                       and i.codcomp  like codcomp||'%'
                       and rownum     = 1;
                exception when no_data_found then
                    v_formusal := null;
                end;

               for r_tstdis in c_tstdis loop
                    v_pctpostr := r_tstdis.pctpostr;
                    v_pctpoend := r_tstdis.pctpoend;
                    v_pctwkstr := r_tstdis.pctwkstr;
                    v_pctwkend := r_tstdis.pctwkend;
                    exit;
                end loop; --c_tstdis

                if v_flggrade in ('1','2') then
                     v_pctincrease := 0;
                    if (nvl(v_amtmaxsa,0) - nvl(v_amtminsa,0)) <> 0 then
                        v_pctincrease := (((i.qtyscor - v_pctwkstr)/(v_pctwkend - v_pctwkstr )) * ((v_pctpoend  - v_pctpostr))) +
                                           (v_pctpostr * i.qtyscor) ;
                        v_pctcal  := v_pctincrease/100;
                    end if;
                else
                    v_pctincrease := v_pctwkend;
                    v_pctcal      := v_pctincrease;
                end if;

               begin
                    select max(dteapend) into v_dteapend
                      from tstdisd
                     where dteyreap   = p_dteyreap
                       and i.codcomp  like codcomp||'%'
                       and exists(select codaplvl
                                  from tempaplvl
                                 where dteyreap = p_dteyreap
                                   and codaplvl = tstdisd.codaplvl);
                exception when no_data_found then
                    v_dteapend := null;
                end;

                v_amtincom := i.amtincom;
                for j in c_ttmovemt loop
                    v_amtincom := j.amtincom;
                    exit;
                end loop;
                ---cal form formusal
                if v_unitcal1 = 'D' then
                    v_amtincom := v_amtincom * 30;
                end if;
                if v_formusal is not null then
                    v_formusalds := v_formusal;
                    v_formusalds := replace(v_formusalds,'{[AMTMID]}',''||v_midpoint||'') ;
                    v_formusalds := replace(v_formusalds,'{[AMTINC]}',''||v_pctcal||'/100') ;
                    v_formusalds := replace(v_formusalds,'{[AMTSAL]}',''||v_amtincom||'') ;
                    v_amtcal     := execute_sql('select '||v_formusalds||' from dual');
                end if;

                v_amtcaln  := v_amtcal;
                v_pctdsaln := nvl(i.pctdsal,0);

                if v_amtcal <> 0  then
                    v_amtdsal := (v_amtcal * v_pctdsaln)/100;
                    v_amtcaln := v_amtcal - v_amtdsal; --4,000
                end if;
                v_amtsaln := nvl(v_amtincom,0) + v_amtcaln; --175,000 + 5,000 = 180,000
                if v_amtsaln > v_amtmaxsa then --180,000 > 180,000
                    v_amtover := v_amtsaln - v_amtmaxsa; --1,000
                end if;

                --Check over salary
                v_amtsalnovr := round(v_amtsaln,2);
                if v_amtincom <= v_amtmaxsa then --68,000 <= 70,000
                  if v_amtsaln > v_amtmaxsa then --190,113 > 180,000
                      --v_amtsalnovr := v_amtincom;
                      if v_flgntover = 'N' then -- can not over
                          v_amtcaln    := v_amtmaxsa - v_amtincom; -- 180,000 - 175,000 = 5,000
                          v_amtsalnovr := v_amtmaxsa;
                          if v_flgpyntover = 'Y' then -- can pay amt over
                              v_amtover := v_amtsaln - v_amtmaxsa; -- 190,113 - 180,000 = 10,113
                              if v_formuntover is not null then
                                  v_formuntoverd := v_formuntover;
                                  v_formuntoverd := replace(v_formuntoverd,'[AMTOVER]',''||v_amtover||'') ;
                                  v_formuntoverd := replace(v_formuntoverd,'{',null) ;
                                  v_formuntoverd := replace(v_formuntoverd,'}',null) ;
                                  v_amtpayover   := execute_sql('select '||v_formuntoverd||' from dual');
                              end if;
                          else
                              v_amtover    := 0;
                              v_amtpayover := 0;
                          end if;
                      end if;
                  end if;
                elsif v_amtincom > v_amtmaxsa then --1,000,000 > 160,000
                  if v_flgover = 'N' then
                      v_amtcalnovr := v_amtcaln;
                      v_amtcaln    := 0;    --old 14,100
                      v_amtsaln    := v_amtincom; --1,000,000
                      v_amtsalnovr := v_amtsaln;
                      if v_flgpyover = 'Y' then
                         v_amtover := v_amtcalnovr;---v_amtsaln - v_amtmaxsa; -- 1,000,000 - 160,000 = 2,000
                          if v_formuover is not null then
                              v_formuoverd := v_formuover;
                              v_formuoverd := replace(v_formuoverd,'[AMTOVER]',''||v_amtover||'') ;
                              v_formuoverd := replace(v_formuoverd,'{',null) ;
                              v_formuoverd := replace(v_formuoverd,'}',null) ;
                              v_amtpayover := execute_sql('select '||v_formuoverd||' from dual');
                          end if;
                      else
                          v_amtover    := 0;
                          v_amtpayover := 0;
                      end if;
                  end if;
                end if; --Check over salary

                v_midpointe   := stdenc(nvl(v_midpoint,0),i.codempid,v_chken);
                v_amtsaloe    := stdenc(nvl(v_amtincom,0),i.codempid,v_chken);
                v_amtcalne    := stdenc(nvl(round(v_amtcaln,2),0),i.codempid,v_chken);
                v_amtsalne    := stdenc(nvl(v_amtsalnovr,0),i.codempid,v_chken);
                v_amtmaxsae   := stdenc(nvl(v_amtmaxsa,0),i.codempid,v_chken);
                v_amtminsae   := stdenc(nvl(v_amtminsa,0),i.codempid,v_chken);
                v_amtovere    := stdenc(nvl(v_amtover,0),i.codempid,v_chken);
                v_amtpayovere := stdenc(nvl(v_amtpayover,0),i.codempid,v_chken);

                if i.process = 'HRAP3XE' then
                    update tappraism set   amtmidsal  = v_midpointe,
                                           amtsalo    = v_amtsaloe,
                                           amtbudg    = v_amtcalne,
                                           amtsaln    = v_amtsalne,
                                           amtceiling = v_amtmaxsae,
                                           amtminsal  = v_amtminsae,
                                           amtover    = v_amtovere,
                                           amtpayover = v_amtpayovere,
                                           pctsal     = v_pctcal,
                                           coduser    = p_coduser
                    where codempid = i.codempid
                     and dteyreap = p_dteyreap
                     and codcomp  = i.codcomp;
                else
                    update tapprais  set   amtmidsal  = v_midpointe,
                                           amtsal     = v_amtsaloe,
                                           amtbudg    = v_amtcalne,
                                           amtsaln    = v_amtsalne,
                                           amtceiling = v_amtmaxsae,
                                           amtminsal  = v_amtminsae,
                                           amtover    = v_amtovere,
                                           amtlums    = v_amtpayovere,
                                           pctcalsal  = v_pctcal,
                                           coduser    = p_coduser
                    where codempid = i.codempid
                     and dteyreap = p_dteyreap;
                end if;

                b_index_sumrec := b_index_sumrec + 1;

            end if; ---if v_flgsecu then
        end loop;   ---for i in c_tappemp loop

        commit;

    end;

end hrap3xe_batch;


/
