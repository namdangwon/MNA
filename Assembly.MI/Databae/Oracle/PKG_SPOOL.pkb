CREATE OR REPLACE PACKAGE BODY HMBMESV1ADM."PKG_SPOOL" 
IS

  PROCEDURE SPL_WRITE
  (
    pch_PLANT    IN  CHAR  ,
    pch_TNAME    IN  CHAR  ,
    pch_KIND     IN  CHAR  ,
    pch_KEY      IN  CHAR  ,
    pch_PROD_DT     IN  CHAR  ,
    pch_STN_CD   IN  CHAR  ,
    pch_STN_SEQ  IN  CHAR  ,
    pch_SUB_SEQ  IN  CHAR  ,
    plg_SPDATA   IN  LONG  ,
    pnu_WPOINT   OUT NUMBER,
    pnu_RETURN   OUT NUMBER,
    pch_ERRMESG  OUT CHAR
  )
    IS

    vnu_STEPNUM         NUMBER  := 0;
    vvc_DETAIL          VARCHAR2(300);
    vch_TERM_ID         CHAR(20):= ' ';

    vnu_WPOINT          NUMBER  := 0;
    vnu_RPOINT          NUMBER  := 0;
    vnu_EPOINT          NUMBER  := 0;
    vnu_SIZE            NUMBER  := 0;
    vnu_FCNT            NUMBER  := 0;

    vch_WPOINT          CHAR(10):= '0';
    vnu_LENDATA         NUMBER  := 0;

    vnu_TempWPOINT      NUMBER  := 0;
    vnu_TempRPOINT      NUMBER  := 0;
    vnu_TempEPOINT      NUMBER  := 0;
    vnu_FULL_FLG        NUMBER  := 0;

    e_ARGS_BAD          EXCEPTION;
    e_SPOOL_FULL        EXCEPTION;
    e_NO_SPOOL          EXCEPTION;
    e_NO_POINT          EXCEPTION;

    CURSOR cur_WRITE1 IS
    SELECT WRITE_POINT, DATA_CNT, FULL_CNT
      FROM C_SPOOL_CTRL_SP
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME
       FOR UPDATE WAIT 3;

    CURSOR cur_WRITE2 IS
    SELECT READ_POINT , EXE_POINT
      FROM C_SPOOL_READ_SP
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    CURSOR cur_WRITEL IS
    SELECT READ_POINT , EXE_POINT
      FROM C_SPOOL_READ_SP
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME
       FOR UPDATE WAIT 3;

    vrec_WRITE1 cur_WRITE1%ROWTYPE ;
    vrec_WRITE2 cur_WRITE2%ROWTYPE ;
    vrec_WRITEL cur_WRITEL%ROWTYPE ;

    BEGIN
        pnu_WPOINT  := 0;
        pnu_RETURN  := 0 ;
        pch_ERRMESG  := '';

        LOOP
            vnu_STEPNUM   := 10;
            IF NOT cur_WRITE1%ISOPEN THEN
              OPEN cur_WRITE1;
            END IF;

            vnu_STEPNUM   := 11;
            FETCH cur_WRITE1 INTO vrec_WRITE1;

            vnu_STEPNUM   := 12;
            IF cur_WRITE1%NOTFOUND THEN
              RAISE e_NO_SPOOL;
            END IF;

            vnu_STEPNUM   := 13;
            vnu_WPOINT := vrec_WRITE1.WRITE_POINT;
            vnu_SIZE   := vrec_WRITE1.DATA_CNT;
            vnu_FCNT   := vrec_WRITE1.FULL_CNT;
            EXIT;

        END LOOP;

        vnu_STEPNUM   := 14;
        IF cur_WRITE1%ISOPEN THEN
          CLOSE cur_WRITE1;
        END IF;

        LOOP
            vnu_STEPNUM   := 15;

            IF NOT cur_WRITE2%ISOPEN THEN
                OPEN cur_WRITE2;
            END IF;

            vnu_STEPNUM   := 16;
            FETCH cur_WRITE2 INTO vrec_WRITE2;

            vnu_STEPNUM   := 17;
            IF cur_WRITE2%NOTFOUND THEN
                RAISE e_NO_POINT;
            END IF;

            vnu_STEPNUM   := 18;
            vnu_RPOINT := vrec_WRITE2.READ_POINT;
            vnu_EPOINT := vrec_WRITE2.EXE_POINT;

            EXIT;
        END LOOP;

        vnu_STEPNUM   := 19;
        IF cur_WRITE2%ISOPEN THEN
            CLOSE cur_WRITE2;
        END IF;

        vnu_STEPNUM   := 20;
        vnu_TempRPOINT := vnu_RPOINT;
        vnu_TempEPOINT := vnu_EPOINT;

        vnu_TempEPOINT := vnu_TempEPOINT - 1;
        IF  vnu_TempEPOINT < 1  THEN
            vnu_TempEPOINT := vnu_TempEPOINT + vnu_SIZE;
        END IF;

        vnu_STEPNUM   := 22;

        IF vnu_TempEPOINT = vnu_WPOINT THEN
            --RAISE e_SPOOL_FULL;
            vnu_FULL_FLG := 1;
            vnu_TempEPOINT := vnu_EPOINT + 1;
            IF  vnu_TempEPOINT > vnu_SIZE  THEN
                vnu_TempEPOINT := vnu_TempEPOINT - vnu_SIZE;
            END IF;
        ELSE
            vnu_TempEPOINT := vnu_EPOINT;
        END IF;

        vnu_TempRPOINT := vnu_TempRPOINT - 1;

        IF  vnu_TempRPOINT < 1  THEN
            vnu_TempRPOINT := vnu_TempRPOINT + vnu_SIZE;
        END IF;

        IF vnu_TempRPOINT = vnu_WPOINT THEN
            --RAISE e_SPOOL_FULL;
            vnu_FULL_FLG := 1;
            vnu_TempRPOINT := vnu_RPOINT + 1;
            IF  vnu_TempRPOINT > vnu_SIZE  THEN
                vnu_TempRPOINT := vnu_TempRPOINT - vnu_SIZE;
            END IF;

        ELSE
            vnu_TempRPOINT := vnu_RPOINT;
        END IF;

        vnu_STEPNUM   := 30;
        vnu_TempWPOINT:= vnu_WPOINT;
        pnu_WPOINT    := vnu_WPOINT;
        vnu_LENDATA   := LENGTH(plg_SPDATA);

        vnu_STEPNUM   := 31;
        UPDATE C_SPOOL_DATA_SP
           SET SP_DATA_ID = pch_KIND    ,
               STN_CD     = pch_STN_CD ,
               SP_KEY     = pch_KEY     ,
               PROD_DT    = pch_PROD_DT ,
               STN_SEQ    = pch_STN_SEQ ,
               SUB_SEQ    = pch_SUB_SEQ ,
               DATA_SIZE  = vnu_LENDATA ,
               ORD_DATA   = plg_SPDATA  ,
               REG_DT     = SYSDATE
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           AND SP_POINT   = vnu_TempWPOINT;

        vnu_STEPNUM   := 32;
        IF SQL%ROWCOUNT <> 1 THEN
             vvc_DETAIL := '' || vnu_TempWPOINT;
             RAISE e_NO_POINT;
        END IF;

        vnu_STEPNUM   := 33;
        vnu_TempWPOINT := vnu_WPOINT + 1 ;

        IF  vnu_TempWPOINT > vnu_SIZE  THEN
            vnu_TempWPOINT := vnu_TempWPOINT - vnu_SIZE;
        END IF;

        vnu_STEPNUM   := 40;
        UPDATE C_SPOOL_CTRL_SP
           SET WRITE_POINT= vnu_TempWPOINT,
               REG_DT     = SYSDATE
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME;

        vnu_STEPNUM   := 41;
        IF SQL%ROWCOUNT <> 1 THEN
             RAISE e_NO_SPOOL;
        END IF;

        IF vnu_FCNT < 1 OR vnu_FULL_FLG != 0 THEN

            vnu_STEPNUM   := 50;
            IF NOT cur_WRITEL%ISOPEN THEN
                OPEN cur_WRITEL;
            END IF;

            vnu_STEPNUM   := 51;
            FETCH cur_WRITEL INTO vrec_WRITEL;

            vnu_STEPNUM   := 52;
            IF cur_WRITEL%NOTFOUND THEN
                RAISE e_NO_POINT;
            END IF;

            vnu_STEPNUM   := 53;
            UPDATE C_SPOOL_READ_SP
               SET READ_POINT   = vnu_TempRPOINT,
                   EXE_POINT    = vnu_TempEPOINT,
                   REG_DT       = SYSDATE
             WHERE PLANT_CD   = pch_PLANT
               AND OUT_DEV_ID = pch_TNAME;

        END IF;

        vnu_STEPNUM   := 54;
        IF cur_WRITEL%ISOPEN THEN
            CLOSE cur_WRITEL;
        END IF;

        vnu_STEPNUM   := 70;
        vch_WPOINT := TO_CHAR(vnu_WPOINT);

        vch_TERM_ID := RTRIM(pch_PLANT) || RTRIM(pch_TNAME);
        --DBMS_ALERT.SIGNAL( vch_TERM_ID, vch_WPOINT ) ;

        vnu_STEPNUM   := 99;
        pnu_RETURN  := 0 ;
        RETURN ;

    EXCEPTION
        WHEN e_ARGS_BAD THEN
            ROLLBACK;
            pnu_RETURN  := 1 ;
            pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(:' || vvc_DETAIL || ')';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WRITE','e_ARGS_BAD',pch_ERRMESG,'EXCEPTION','P');

        WHEN e_NO_SPOOL THEN
            -- ROLLBACK; --// The device is skipped(no rollback) if not found -- NO DATA FOUND C_SPOOL_CTRL_SP
            pnu_RETURN  := 2 ;
            pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WRITE','e_NO_SPOOL',pch_ERRMESG,'EXCEPTION','P');

        WHEN e_NO_POINT THEN
            -- ROLLBACK;    -- NO DATA FOUND C_SPOOL_DATA_SP
            pnu_RETURN  := 3 ;
            pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WRITE','e_NO_POINT',pch_ERRMESG,'EXCEPTION','P');

        WHEN e_SPOOL_FULL THEN
            ROLLBACK;
            pnu_RETURN  := 4 ;
            pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WRITE','e_SPOOL_FULL',pch_ERRMESG,'EXCEPTION','P');

        WHEN OTHERS THEN
            ROLLBACK;
            IF SQLCODE = -30006 THEN
                pnu_RETURN  := -9 ;
            ELSE
                pnu_RETURN  := -1 ;
                CHK_DB_CONNECT(pnu_RETURN);
            END IF;

            pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WRITE','OTHERS',pch_ERRMESG,'EXCEPTION','P');

            IF cur_WRITEL%ISOPEN THEN
                CLOSE cur_WRITEL;
            END IF;

            IF cur_WRITE2%ISOPEN THEN
                CLOSE cur_WRITE2;
            END IF;

            IF cur_WRITE1%ISOPEN THEN
                CLOSE cur_WRITE1;
            END IF;

            RETURN ;

    END SPL_WRITE;

    PROCEDURE SPL_TERMINIT
    (
        pch_PLANT    IN     CHAR,
        pch_TNAME    IN     CHAR,
        pnu_RETURN   OUT    NUMBER,
        pch_ERRMESG  OUT    CHAR
    )
    IS
        vnu_STEPNUM         NUMBER  := 0;
        vch_TERM_ID         CHAR(20):= ' ';
    BEGIN
        pnu_RETURN  := 0 ;
        pch_ERRMESG  := '';

        vnu_STEPNUM := 1;
        vch_TERM_ID := RTRIM(pch_PLANT) || RTRIM(pch_TNAME);
        --DBMS_ALERT.REGISTER( vch_TERM_ID ) ;

        pnu_RETURN  := 0 ;
        RETURN ;

    EXCEPTION
        WHEN OTHERS THEN
            pnu_RETURN  := -1 ;
            CHK_DB_CONNECT(pnu_RETURN);
            pch_ERRMESG := 'MODOULE:SPL_TERMINIT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_TERMINIT','OTHERS',pch_ERRMESG,'EXCEPTION','P');
            RETURN ;

    END SPL_TERMINIT;

    PROCEDURE SPL_READ
    (
        pch_PLANT     IN  CHAR     ,
        pch_TNAME     IN  CHAR     ,
        pnu_UPTEPOINT IN  NUMBER   ,
        pnu_RPOINT    OUT NUMBER   ,
        pch_KIND      OUT CHAR     ,
        pch_KEY       OUT CHAR     ,
        pch_PROD_DT   OUT CHAR     ,
        pch_STN_CD    OUT CHAR     ,
        pch_STN_SEQ   OUT CHAR     ,
        pch_SUB_SEQ   OUT CHAR     ,
        pnu_LENGTH    OUT NUMBER   ,
        plg_SPDATA    OUT LONG     ,
        pnu_RETURN    OUT NUMBER   ,
        pch_ERRMESG   OUT CHAR
    )
    IS
        vnu_STEPNUM         NUMBER  := 0;
        vvc_DETAIL          VARCHAR2(300);

        vnu_WPOINT          NUMBER  := 0;
        vnu_RPOINT          NUMBER  := 0;
        vnu_EPOINT          NUMBER  := 0;
        vnu_SIZE            NUMBER  := 0;
        vnu_TempRPOINT      NUMBER  := 0;
        log_txt             varchar2(4000);


        CURSOR cur_CTRLR IS
            SELECT WRITE_POINT, DATA_CNT
              FROM C_SPOOL_CTRL_SP
             WHERE PLANT_CD   = pch_PLANT
               AND OUT_DEV_ID = pch_TNAME;

        CURSOR cur_READ1 IS
            SELECT READ_POINT, EXE_POINT
              FROM C_SPOOL_READ_SP
             WHERE PLANT_CD   = pch_PLANT
               AND OUT_DEV_ID = pch_TNAME
               FOR UPDATE WAIT 3;

        CURSOR cur_READ2 IS
            SELECT SP_POINT, SP_DATA_ID, STN_CD, SP_KEY, PROD_DT, STN_SEQ, SUB_SEQ, DATA_SIZE, ORD_DATA
              FROM C_SPOOL_DATA_SP
             WHERE PLANT_CD   = pch_PLANT
               AND OUT_DEV_ID = pch_TNAME
               AND SP_POINT   = vnu_RPOINT;

        vrec_READ1     cur_READ1%ROWTYPE ;
        vrec_READ2     cur_READ2%ROWTYPE ;
        vrec_CTRLR     cur_CTRLR%ROWTYPE ;

        e_ARGS_BAD          EXCEPTION;
        e_SPOOL_FULL        EXCEPTION;
        e_NO_SPOOL          EXCEPTION;
        e_NO_POINT          EXCEPTION;
        e_NO_DATA           EXCEPTION;

    BEGIN

        pnu_RPOINT := 0;
        pch_KIND   := '';
        pch_KEY    := '';
        pnu_LENGTH := 0;
        pnu_RETURN := 0;
        pch_ERRMESG:= '';

        LOOP
            vnu_STEPNUM   := 11;
            IF NOT cur_CTRLR%ISOPEN THEN
                OPEN cur_CTRLR;
            END IF;

            vnu_STEPNUM   := 12;
            FETCH cur_CTRLR INTO vrec_CTRLR;

            vnu_STEPNUM   := 13;
            IF cur_CTRLR%NOTFOUND THEN
                RAISE e_NO_SPOOL;
            END IF;

            vnu_STEPNUM   := 14;
            IF NOT cur_READ1%ISOPEN THEN
                OPEN cur_READ1;
            END IF;

            vnu_STEPNUM   := 15;
            FETCH cur_READ1 INTO vrec_READ1;

            vnu_STEPNUM   := 16;
            IF cur_READ1%NOTFOUND THEN
                RAISE e_NO_POINT;
            END IF;

            vnu_WPOINT := vrec_CTRLR.WRITE_POINT;
            vnu_RPOINT := vrec_READ1.READ_POINT;
            vnu_SIZE   := vrec_CTRLR.DATA_CNT;

            IF vnu_RPOINT = vnu_WPOINT THEN
                RAISE e_NO_DATA;
            END IF;

            LOOP
                vnu_STEPNUM   := 20;
                IF NOT cur_READ2%ISOPEN THEN
                     OPEN cur_READ2;
                END IF;

                vnu_STEPNUM   := 21;
                FETCH cur_READ2 INTO vrec_READ2;

                vnu_STEPNUM   := 22;
                IF cur_READ2%NOTFOUND THEN
                  vvc_DETAIL := '' || vnu_RPOINT;
                  RAISE e_NO_POINT;
                END IF;

                vnu_STEPNUM   := 30;
                vnu_TempRPOINT := vnu_RPOINT + 1 ;

                IF  vnu_TempRPOINT > vnu_SIZE  THEN
                     vnu_TempRPOINT := vnu_TempRPOINT - vnu_SIZE;
                END IF;

                pnu_RPOINT := vnu_RPOINT;
                vnu_RPOINT := vnu_TempRPOINT;

                vnu_STEPNUM   := 31;
                IF pnu_UPTEPOINT = 1 THEN
                    BEGIN
                        UPDATE C_SPOOL_READ_SP
                           SET READ_POINT = vnu_RPOINT,
                               EXE_POINT  = vnu_RPOINT,
                               REG_DT       = SYSDATE
                         WHERE PLANT_CD   = pch_PLANT
                           AND OUT_DEV_ID = pch_TNAME;
                    EXCEPTION
                    WHEN OTHERS THEN
                        log_txt := 'ERR_NO : '|| TRIM(TO_CHAR(ABS(SQLCODE),'09999')) || ', MSG :' || SUBSTR(SQLERRM,1,1800);
                        --PKG_COM_LOGMSG.PRC_SND_LOG('E','SPOOL_TEST','STEP1 ',log_txt,'SPOOL_PKG','P');

                    END;

                ELSE
                    BEGIN
                        UPDATE C_SPOOL_READ_SP
                           SET READ_POINT = vnu_RPOINT,
                               REG_DT       = SYSDATE
                         WHERE PLANT_CD   = pch_PLANT
                           AND OUT_DEV_ID = pch_TNAME;
                    EXCEPTION
                        WHEN OTHERS THEN
                        log_txt := 'ERR_NO : '|| TRIM(TO_CHAR(ABS(SQLCODE),'09999')) || ', MSG :' || SUBSTR(SQLERRM,1,1800);
                        --PKG_COM_LOGMSG.PRC_SND_LOG('E','SPOOL_TEST','STEP2',log_txt,'SPOOL_PKG','P');

                    END;
                END IF;

                vnu_STEPNUM   := 32;
                IF SQL%ROWCOUNT <> 1 THEN
                    vvc_DETAIL := '' || vnu_RPOINT;
                    RAISE e_NO_POINT;
                END IF;

                vnu_STEPNUM   := 33;
                pnu_RPOINT := vrec_READ2.SP_POINT;
                pch_KIND   := vrec_READ2.SP_DATA_ID;
                pch_KEY    := vrec_READ2.SP_KEY;
                pch_PROD_DT:= vrec_READ2.PROD_DT;
                pch_STN_CD := vrec_READ2.STN_CD;
                pch_STN_SEQ:= vrec_READ2.STN_SEQ;
                pch_SUB_SEQ:= vrec_READ2.SUB_SEQ;
                pnu_LENGTH := vrec_READ2.DATA_SIZE;
                plg_SPDATA := vrec_READ2.ORD_DATA;

                IF pch_KIND='IT' THEN
                    pch_KIND := 'IN';
                END IF;

                IF pch_KIND='ST' THEN
                    pch_KIND := 'SN';
                END IF;

                vnu_STEPNUM   := 34;
                EXIT;

            END LOOP;

            IF cur_READ2%ISOPEN THEN
                CLOSE cur_READ2;
            END IF;

            vnu_STEPNUM   := 40;
            EXIT;
        END LOOP;

        IF cur_CTRLR%ISOPEN THEN
          CLOSE cur_CTRLR;
        END IF;

        IF cur_READ1%ISOPEN THEN
            CLOSE cur_READ1;
        END IF;

        vnu_STEPNUM   := 100;
        pnu_RETURN  := 0 ;
        RETURN;

    EXCEPTION
    WHEN e_ARGS_BAD THEN
        ROLLBACK;
        pnu_RETURN  := 1 ;
        pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(:' || vvc_DETAIL || ')';
        pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
        --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_READ','e_ARGS_BAD',pch_ERRMESG,'EXCEPTION','P');

    WHEN e_NO_SPOOL THEN
        ROLLBACK;
        pnu_RETURN  := 2 ;
        pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
        pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
        --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_READ','e_NO_SPOOL',pch_ERRMESG,'EXCEPTION','P');

    WHEN e_NO_POINT THEN
        ROLLBACK;
        pnu_RETURN  := 3 ;
        pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
        pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
        --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_READ','e_NO_POINT',pch_ERRMESG,'EXCEPTION','P');

    WHEN e_SPOOL_FULL THEN
        ROLLBACK;
        pnu_RETURN  := 4 ;
        pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL';
        pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
        --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_READ','e_SPOOL_FULL',pch_ERRMESG,'EXCEPTION','P');

    WHEN e_NO_DATA THEN
        ROLLBACK;
        pnu_RETURN  := 100 ;
        pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:READING DATA IS NOTHING';
        pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
        --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_READ','e_NO_DATA',pch_ERRMESG,'EXCEPTION','P');

    WHEN OTHERS THEN
        ROLLBACK;
        IF SQLCODE = -30006 THEN
            pnu_RETURN  := -9 ;
        ELSE
            pnu_RETURN  := -1 ;
            CHK_DB_CONNECT(pnu_RETURN);
        END IF;

        pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
        pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
        --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_TERMINIT','OTHERS',pch_ERRMESG,'EXCEPTION','P');

        IF cur_READ2%ISOPEN THEN
            CLOSE cur_READ2;
        END IF;

        IF cur_CTRLR%ISOPEN THEN
            CLOSE cur_CTRLR;
        END IF;

        IF cur_READ1%ISOPEN THEN
            CLOSE cur_READ1;
        END IF;

        RETURN ;

    END SPL_READ;

    PROCEDURE SPL_RETRIEVE
    (
        pch_PLANT    IN     CHAR  ,
        pch_TNAME    IN     CHAR  ,
        pnu_EPOINT   IN OUT  NUMBER,
        pch_KIND     OUT CHAR     ,
        pch_KEY      OUT CHAR     ,
        pch_PROD_DT        OUT        CHAR    ,
        pch_STN_CD         OUT        CHAR    ,
        pch_STN_SEQ        OUT        CHAR    ,
        pch_SUB_SEQ        OUT        CHAR    ,
        pnu_LENGTH   OUT NUMBER   ,
        plg_SPDATA   OUT LONG     ,
        pnu_RETURN   OUT NUMBER   ,
        pch_ERRMESG  OUT CHAR
    )
    IS
        vnu_STEPNUM         NUMBER  := 0;
        vvc_DETAIL          VARCHAR2(300);

        vnu_WPOINT          NUMBER  := 0;
        vnu_RPOINT          NUMBER  := 0;
        vnu_EPOINT          NUMBER  := 0;
        vnu_SIZE            NUMBER  := 0;

        vnu_SPOINT          NUMBER  := 0;
        vnu_SCOUNT          NUMBER  := 0;
        vnu_RETR_PNT        NUMBER  := 0;
        vnu_RCOUNT          NUMBER  := 0;
        vnu_ROOPCNT         NUMBER  := 0;

        vnu_TempEPOINT      NUMBER  := 0;

        CURSOR cur_RETR1 IS
        SELECT A.WRITE_POINT, B.READ_POINT, B.EXE_POINT, A.DATA_CNT
          FROM C_SPOOL_CTRL_SP A, C_SPOOL_READ_SP B
         WHERE A.PLANT_CD   = pch_PLANT
           AND A.OUT_DEV_ID = pch_TNAME
           AND A.PLANT_CD   = B.PLANT_CD
           AND A.OUT_DEV_ID = B.OUT_DEV_ID;

        CURSOR cur_RETR2 IS
        SELECT SP_POINT, SP_DATA_ID, STN_CD, SP_KEY, PROD_DT, STN_SEQ, SUB_SEQ, DATA_SIZE, ORD_DATA
          FROM C_SPOOL_DATA_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           AND SP_POINT   = vnu_EPOINT;

        CURSOR cur_RETR3 IS
        SELECT SP_POINT, SP_DATA_ID
          FROM C_SPOOL_DATA_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           AND SP_POINT   BETWEEN vnu_RETR_PNT-200 AND vnu_RETR_PNT
         ORDER BY SP_POINT DESC;

        vrec_RETR1     cur_RETR1%ROWTYPE ;
        vrec_RETR2     cur_RETR2%ROWTYPE ;
        vrec_RETR3     cur_RETR3%ROWTYPE ;

        e_ARGS_BAD          EXCEPTION;
        e_SPOOL_FULL        EXCEPTION;
        e_NO_SPOOL          EXCEPTION;
        e_NO_POINT          EXCEPTION;
        e_NO_DATA           EXCEPTION;

    BEGIN

        pch_KIND   := '';
        pch_KEY    := '';
        pnu_LENGTH := 0;
        pnu_RETURN := 0;
        pch_ERRMESG:= '';

        LOOP

            vnu_STEPNUM   := 10;
            IF NOT cur_RETR1%ISOPEN THEN
                OPEN cur_RETR1;
            END IF;

            vnu_STEPNUM   := 11;
            FETCH cur_RETR1 INTO vrec_RETR1;

            vnu_STEPNUM   := 12;
            IF cur_RETR1%NOTFOUND THEN
                RAISE e_NO_SPOOL;
            END IF;

            vnu_STEPNUM   := 13;
            vnu_WPOINT := vrec_RETR1.WRITE_POINT;
            vnu_RPOINT := vrec_RETR1.READ_POINT;
            vnu_EPOINT := vrec_RETR1.EXE_POINT;
            vnu_SIZE   := vrec_RETR1.DATA_CNT;

            --DBMS_OUTPUT.PUT_LINE ('==>LOOP vnu_RETR_PNT:' || vnu_RETR_PNT ) ;

            vnu_STEPNUM   := 20;
            IF pnu_EPOINT > 0 THEN

                vnu_EPOINT := pnu_EPOINT + 1;
                IF vnu_EPOINT > vnu_SIZE THEN
                    vnu_EPOINT := 1;
                END IF;

                vnu_STEPNUM   := 21;
                IF vnu_EPOINT = vnu_RPOINT OR vnu_EPOINT = vnu_WPOINT THEN
                    RAISE e_NO_DATA;
                END IF;

            ELSE
                vnu_STEPNUM   := 31;
                vnu_SPOINT    := 0;
                vnu_RCOUNT    := pnu_EPOINT*-1;
                IF vnu_RCOUNT > vnu_SIZE THEN
                    vnu_RCOUNT := vnu_SIZE;
                END IF;

                --S.H.BAE_20060901_MOD_START
                --vnu_RETR_PNT := vnu_EPOINT;
                vnu_RETR_PNT := vnu_EPOINT-1;
                IF vnu_RETR_PNT < 1 THEN
                    vnu_RETR_PNT := vnu_SIZE;
                END IF;
                vnu_EPOINT := 0;
                --S.H.BAE_20060901_MOD_END

                LOOP
                    vnu_STEPNUM   := 32;
                    IF NOT cur_RETR3%ISOPEN THEN
                        OPEN cur_RETR3;
                    END IF;

                    vnu_STEPNUM   := 33;
                    FETCH cur_RETR3 INTO vrec_RETR3;

                    IF cur_RETR3%NOTFOUND THEN
                        CLOSE cur_RETR3;

                        vnu_STEPNUM   :=34;
                        IF vnu_SPOINT = 1 THEN
                            vnu_RETR_PNT := vnu_SIZE;
                        ELSE
                            vnu_RETR_PNT := vnu_RETR_PNT - 200;
                        END IF;
                    ELSE
                        vnu_ROOPCNT := vnu_ROOPCNT + 1;

                        vnu_STEPNUM   := 35;
                        --J.Y.PARK_20070330_CHG_START
                        --IF vrec_RETR3.SP_DATA_ID = ' ' THEN
                          IF SUBSTR(vrec_RETR3.SP_DATA_ID,1,1) = ' ' THEN
                        --J.Y.PARK_20070330_CHG_END
                        EXIT;
                    END IF;


                    vnu_SPOINT := vrec_RETR3.SP_POINT;
                    vnu_EPOINT := vnu_SPOINT;

                    IF vnu_SPOINT != vnu_WPOINT AND
                        (vrec_RETR3.SP_DATA_ID = 'SN' OR vrec_RETR3.SP_DATA_ID = 'SR' OR vrec_RETR3.SP_DATA_ID = 'ST') THEN
                        vnu_SCOUNT := vnu_SCOUNT + 1;
                    END IF;

                    vnu_STEPNUM   := 37;
                    IF vnu_SPOINT = vnu_WPOINT THEN
                        EXIT;
                    END IF;

                    vnu_STEPNUM   := 38;
                    EXIT WHEN vnu_SCOUNT >= vnu_RCOUNT;

                    vnu_STEPNUM   :=39;
                    EXIT WHEN vnu_ROOPCNT > vnu_SIZE;
                    END IF;

                END LOOP;

                IF cur_RETR3%ISOPEN THEN
                    CLOSE cur_RETR3;
                END IF;

            END IF;

            --S.H.BAE_20060901_MOD_START
            --vnu_STEPNUM   := 40;
            --IF vnu_EPOINT = vnu_RPOINT THEN
            --RAISE e_NO_DATA;
            --END IF;

            IF vnu_EPOINT < 1 THEN
                RAISE e_NO_DATA;
            END IF;
            --S.H.BAE_20060901_MOD_END

            vnu_STEPNUM   := 41;
            IF NOT cur_RETR2%ISOPEN THEN
                 OPEN cur_RETR2;
            END IF;

            LOOP
                vnu_STEPNUM   := 50;
                FETCH cur_RETR2 INTO vrec_RETR2;

                vnu_STEPNUM   := 51;
                IF cur_RETR2%NOTFOUND THEN
                    vvc_DETAIL := '' || vnu_EPOINT;
                    RAISE e_NO_POINT;
                END IF;

                vnu_STEPNUM   := 52;
                pnu_EPOINT := vrec_RETR2.SP_POINT;
                pch_KIND   := vrec_RETR2.SP_DATA_ID;
                pch_STN_CD := vrec_RETR2.STN_CD;
                pch_KEY    := vrec_RETR2.SP_KEY;
                pch_PROD_DT:= vrec_RETR2.PROD_DT;
                pch_STN_SEQ:= vrec_RETR2.STN_SEQ;
                pch_SUB_SEQ:= vrec_RETR2.SUB_SEQ;
                pnu_LENGTH := vrec_RETR2.DATA_SIZE;
                plg_SPDATA := vrec_RETR2.ORD_DATA;

                IF pch_KIND='IT' THEN
                    pch_KIND := 'IN';
                END IF;

                IF pch_KIND='ST' THEN
                    pch_KIND := 'SN';
                END IF;

                vnu_STEPNUM   := 53;
                EXIT;

            END LOOP;

            vnu_STEPNUM   := 60;
            IF cur_RETR2%ISOPEN THEN
                CLOSE cur_RETR2;
            END IF;

            vnu_STEPNUM   := 61;
            EXIT;

        END LOOP;

        vnu_STEPNUM   := 91;
        IF cur_RETR1%ISOPEN THEN
            CLOSE cur_RETR1;
        END IF;

        vnu_STEPNUM   := 100;
        pnu_RETURN  := 0 ;
        RETURN;

    EXCEPTION
        WHEN e_ARGS_BAD THEN
            pnu_RETURN  := 1 ;
            pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(:' || vvc_DETAIL || ')';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_RETRIEVE','e_ARGS_BAD',pch_ERRMESG,'EXCEPTION','P');

        WHEN e_NO_SPOOL THEN
            pnu_RETURN  := 2 ;
            pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_RETRIEVE','e_NO_SPOOL',pch_ERRMESG,'EXCEPTION','P');

        WHEN e_NO_POINT THEN
            pnu_RETURN  := 3 ;
            pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_RETRIEVE','e_NO_POINT',pch_ERRMESG,'EXCEPTION','P');

        WHEN e_SPOOL_FULL THEN
            pnu_RETURN  := 4 ;
            pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_RETRIEVE','e_SPOOL_FULL',pch_ERRMESG,'EXCEPTION','P');

        WHEN e_NO_DATA THEN
            pnu_RETURN  := 100 ;
            pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:READING DATA IS NOTHING';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_RETRIEVE','e_NO_DATA',pch_ERRMESG,'EXCEPTION','P');

        WHEN OTHERS THEN
            pnu_RETURN  := -1 ;
            CHK_DB_CONNECT(pnu_RETURN);
            pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_RETRIEVE','OTHERS',pch_ERRMESG,'EXCEPTION','P');

            IF cur_RETR3%ISOPEN THEN
              CLOSE cur_RETR3;
            END IF;

            IF cur_RETR2%ISOPEN THEN
              CLOSE cur_RETR2;
            END IF;

            IF cur_RETR1%ISOPEN THEN
              CLOSE cur_RETR1;
            END IF;

            RETURN ;

    END SPL_RETRIEVE;

  PROCEDURE SPL_ALL_RETRIEVE
  ( pch_PLANT       IN      CHAR  ,
    pch_TNAME       IN      CHAR  ,
    pnu_EPOINT      IN OUT  NUMBER,
    pch_KIND        OUT     CHAR     ,
    pch_KEY         OUT     CHAR     ,
    pch_PROD_DT     OUT     CHAR    ,
    pch_STN_CD      OUT     CHAR    ,
    pch_STN_SEQ     OUT     CHAR    ,
    pch_SUB_SEQ     OUT     CHAR    ,
    pnu_LENGTH      OUT     NUMBER   ,
    plg_SPDATA      OUT     LONG     ,
    pnu_RETURN      OUT     NUMBER   ,
    pch_ERRMESG     OUT     CHAR
  )
    IS
    vnu_STEPNUM             NUMBER  := 0;
    vvc_DETAIL              VARCHAR2(300);

    vnu_WPOINT              NUMBER  := 0;
    vnu_RPOINT              NUMBER  := 0;
    vnu_EPOINT              NUMBER  := 0;
    vnu_SIZE                NUMBER  := 0;

    vnu_SPOINT              NUMBER  := 0;
    vnu_SCOUNT              NUMBER  := 0;
    vnu_RETR_PNT            NUMBER  := 0;
    vnu_RCOUNT              NUMBER  := 0;
    vnu_ROOPCNT             NUMBER  := 0;

    vnu_TempEPOINT          NUMBER  := 0;

    CURSOR cur_RETR_ALL1 IS
        SELECT A.WRITE_POINT, B.READ_POINT, B.EXE_POINT, A.DATA_CNT
          FROM C_SPOOL_CTRL_SP A, C_SPOOL_READ_SP B
         WHERE A.PLANT_CD   = pch_PLANT
           AND A.OUT_DEV_ID = pch_TNAME
           AND A.PLANT_CD   = B.PLANT_CD
           AND A.OUT_DEV_ID = B.OUT_DEV_ID;

    CURSOR cur_RETR_ALL2 IS
        SELECT SP_POINT, SP_DATA_ID, STN_CD, SP_KEY, PROD_DT, STN_SEQ, SUB_SEQ, DATA_SIZE, ORD_DATA
          FROM C_SPOOL_DATA_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           AND SP_POINT   = vnu_EPOINT;

    CURSOR cur_RETR_ALL3 IS
        SELECT SP_POINT, SP_DATA_ID
          FROM C_SPOOL_DATA_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           AND SP_POINT   BETWEEN vnu_RETR_PNT-200 AND vnu_RETR_PNT
         ORDER BY SP_POINT DESC;

    vrec_RETR_ALL1     cur_RETR_ALL1%ROWTYPE ;
    vrec_RETR_ALL2     cur_RETR_ALL2%ROWTYPE ;
    vrec_RETR_ALL3     cur_RETR_ALL3%ROWTYPE ;

    e_ARGS_BAD          EXCEPTION;
    e_SPOOL_FULL        EXCEPTION;
    e_NO_SPOOL          EXCEPTION;
    e_NO_POINT          EXCEPTION;
    e_NO_DATA           EXCEPTION;

  BEGIN

  pch_KIND   := '';
  pch_KEY    := '';
  pnu_LENGTH := 0;
  pnu_RETURN := 0;
  pch_ERRMESG:= '';

  LOOP

    vnu_STEPNUM   := 10;
    IF NOT cur_RETR_ALL1%ISOPEN THEN
      OPEN cur_RETR_ALL1;
    END IF;

    vnu_STEPNUM   := 11;
    FETCH cur_RETR_ALL1 INTO vrec_RETR_ALL1;

    vnu_STEPNUM   := 12;
    IF cur_RETR_ALL1%NOTFOUND THEN
      RAISE e_NO_SPOOL;
    END IF;

    vnu_STEPNUM   := 13;
    vnu_WPOINT := vrec_RETR_ALL1.WRITE_POINT;
    vnu_RPOINT := vrec_RETR_ALL1.READ_POINT;
    vnu_EPOINT := vrec_RETR_ALL1.EXE_POINT;
    vnu_SIZE   := vrec_RETR_ALL1.DATA_CNT;

    vnu_STEPNUM   := 20;
    IF pnu_EPOINT > 0 THEN

      pnu_EPOINT := pnu_EPOINT + 1;
      IF pnu_EPOINT > vnu_SIZE THEN
        pnu_EPOINT := 1;
      END IF;

      vnu_STEPNUM   := 21;
      --IF pnu_EPOINT = vnu_RPOINT OR pnu_EPOINT = vnu_WPOINT THEN
      IF pnu_EPOINT = vnu_WPOINT THEN
        RAISE e_NO_DATA;
      END IF;

      vnu_STEPNUM   := 22;
      vnu_EPOINT := pnu_EPOINT;

    ELSE

      vnu_STEPNUM   := 31;
      vnu_RCOUNT := pnu_EPOINT*-1;
      IF vnu_RCOUNT > vnu_SIZE THEN
        vnu_RCOUNT := vnu_SIZE;
      END IF;

      vnu_RETR_PNT := vnu_EPOINT-1;
      IF vnu_RETR_PNT < 1 THEN
        vnu_RETR_PNT := vnu_SIZE;
      END IF;

      LOOP

        vnu_STEPNUM   := 32;
        IF NOT cur_RETR_ALL3%ISOPEN THEN
          OPEN cur_RETR_ALL3;
        END IF;

        FETCH cur_RETR_ALL3 INTO vrec_RETR_ALL3;

        IF cur_RETR_ALL3%NOTFOUND THEN
          CLOSE cur_RETR_ALL3;

          vnu_STEPNUM   := 34;
          IF vnu_SPOINT = 1 THEN
            vnu_RETR_PNT := vnu_SIZE;
          ELSE
            vnu_RETR_PNT := vnu_RETR_PNT - 200;
          END IF;

        ELSE
          vnu_ROOPCNT := vnu_ROOPCNT + 1;

          vnu_STEPNUM   := 35;
          IF vrec_RETR_ALL3.SP_DATA_ID = ' ' THEN
            EXIT;
          END IF;

          vnu_SPOINT := vrec_RETR_ALL3.SP_POINT;

          vnu_EPOINT := vnu_SPOINT;

          vnu_SCOUNT := vnu_SCOUNT + 1;

          vnu_STEPNUM   := 36;
          IF vnu_SPOINT = vnu_WPOINT THEN
            EXIT;
          END IF;

          vnu_STEPNUM   := 37;
          EXIT WHEN vnu_SCOUNT >= vnu_RCOUNT;

          vnu_STEPNUM   :=38;
          EXIT WHEN vnu_ROOPCNT > vnu_SIZE;
        END IF;

      END LOOP;

      IF cur_RETR_ALL3%ISOPEN THEN
        CLOSE cur_RETR_ALL3;
      END IF;

    END IF;

    vnu_STEPNUM   := 45;
    IF NOT cur_RETR_ALL2%ISOPEN THEN
         OPEN cur_RETR_ALL2;
    END IF;

    LOOP

      vnu_STEPNUM   := 46;
      FETCH cur_RETR_ALL2 INTO vrec_RETR_ALL2;

      vnu_STEPNUM   := 47;
      IF cur_RETR_ALL2%NOTFOUND THEN
        vvc_DETAIL := '' || vnu_EPOINT;
        RAISE e_NO_POINT;
      END IF;

      vnu_STEPNUM   := 48;
      pnu_EPOINT := vrec_RETR_ALL2.SP_POINT;
      pch_KIND   := vrec_RETR_ALL2.SP_DATA_ID;
      pch_KEY    := vrec_RETR_ALL2.SP_KEY;
      pch_PROD_DT:= vrec_RETR_ALL2.PROD_DT;
      pch_STN_CD := vrec_RETR_ALL2.STN_CD;
      pch_STN_SEQ:= vrec_RETR_ALL2.STN_SEQ;
      pch_SUB_SEQ:= vrec_RETR_ALL2.SUB_SEQ;
      pnu_LENGTH := vrec_RETR_ALL2.DATA_SIZE;
      plg_SPDATA := vrec_RETR_ALL2.ORD_DATA;

      vnu_STEPNUM   := 49;
      EXIT;

    END LOOP;

    IF cur_RETR_ALL2%ISOPEN THEN
      CLOSE cur_RETR_ALL2;
    END IF;

    vnu_STEPNUM   := 60;
    EXIT;
  END LOOP;

  IF cur_RETR_ALL1%ISOPEN THEN
    CLOSE cur_RETR_ALL1;
  END IF;

  vnu_STEPNUM   := 100;
  pnu_RETURN  := 0 ;
  RETURN;

  EXCEPTION
    WHEN e_ARGS_BAD THEN
      pnu_RETURN  := 1 ;
      pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_SPOOL THEN
      pnu_RETURN  := 2 ;
      pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_POINT THEN
      pnu_RETURN  := 3 ;
      pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_SPOOL_FULL THEN
      pnu_RETURN  := 4 ;
      pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_DATA THEN
      pnu_RETURN  := 100 ;
      pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:READING DATA IS NOTHING';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN OTHERS THEN
      pnu_RETURN  := -1 ;
      CHK_DB_CONNECT(pnu_RETURN);
      pch_ERRMESG := 'MODOULE:SPL_READ, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;

    IF cur_RETR_ALL3%ISOPEN THEN
      CLOSE cur_RETR_ALL3;
    END IF;

    IF cur_RETR_ALL2%ISOPEN THEN
      CLOSE cur_RETR_ALL2;
    END IF;

    IF cur_RETR_ALL1%ISOPEN THEN
      CLOSE cur_RETR_ALL1;
    END IF;

    RETURN ;

  END SPL_ALL_RETRIEVE;

  PROCEDURE SPL_CREATE
  ( pch_PLANT    IN     CHAR        ,
    pch_TNAME    IN     CHAR        ,
    pnu_SIZE     IN     NUMBER   ,
    pnu_THREHOLD IN     NUMBER   ,
    pnu_RETURN   OUT    NUMBER   ,
    pch_ERRMESG  OUT    CHAR
  )
    IS
    vnu_STEPNUM         NUMBER  := 0;

    vnu_POINT           NUMBER  := 1;

  BEGIN
    pnu_RETURN  := 0 ;
    pch_ERRMESG  := '';

    vnu_STEPNUM := 10;
    INSERT INTO C_SPOOL_CTRL_SP(
      PLANT_CD   , OUT_DEV_ID , WRITE_POINT,
      DATA_CNT   , FULL_CNT   , ALARM_CNT,
      USAGE      , REG_DT
    ) VALUES (
      pch_PLANT  , pch_TNAME  , 1          ,
      pnu_SIZE   , pnu_THREHOLD,         10,
      '0', SYSDATE
    );

    INSERT INTO C_SPOOL_READ_SP(
      PLANT_CD   , OUT_DEV_ID ,
      READ_POINT , EXE_POINT  , REG_DT
    ) VALUES (
      pch_PLANT  , pch_TNAME  ,
      1          , 1          , SYSDATE
    );

    vnu_STEPNUM := 20;
    DELETE FROM C_SPOOL_DATA_SP
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    vnu_STEPNUM := 30;
    LOOP

      vnu_STEPNUM := 30;
      INSERT INTO C_SPOOL_DATA_SP(
        PLANT_CD   , OUT_DEV_ID , SP_POINT   ,
        SP_DATA_ID , STN_CD        , SP_KEY     , PROD_DT    ,
        STN_SEQ    , SUB_SEQ    , DATA_SIZE  , ORD_DATA   ,
        REG_DT
      ) VALUES (
        pch_PLANT  , pch_TNAME  , vnu_POINT  ,
        ' '        , ' '        , ' '        , ' '        ,
        ' '        , ' '        , 0          , ' '        ,
        SYSDATE
      );

      IF vnu_POINT >= pnu_SIZE THEN
           EXIT;
      ELSE
           vnu_POINT := vnu_POINT + 1;
      END IF;

    END LOOP;

    pnu_RETURN  := 0 ;
    RETURN;

  EXCEPTION
    WHEN OTHERS THEN
      pnu_RETURN  := -1 ;
      CHK_DB_CONNECT(pnu_RETURN);
      pch_ERRMESG := 'MODOULE:SPL_CREATE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
      ROLLBACK;
    RETURN ;

  END SPL_CREATE;


  PROCEDURE SPL_INCREASE
  ( pch_PLANT    IN  CHAR    ,
    pch_TNAME    IN  CHAR    ,
    pnu_SIZE     IN  NUMBER  ,
    pch_ENFORCE     IN  CHAR    ,
    pnu_RETURN   OUT NUMBER  ,
    pch_ERRMESG  OUT CHAR
  )
    IS
    vnu_STEPNUM         NUMBER  := 0;
    vvc_DETAIL          VARCHAR2(300);

    vnu_WPOINT          NUMBER  := 0;
    vnu_RPOINT          NUMBER  := 0;
    vnu_EPOINT          NUMBER  := 0;
    vnu_SIZE            NUMBER  := 0;

    nuvTEMPPOINT  NUMBER;
    nuvRepeat     NUMBER;            -- Temporary spool point
    nuvInsertCount NUMBER;
    nuvRestoreEnd NUMBER;
    nuvRealCount  NUMBER;

    nuvReadPoint  NUMBER;
    nuvWritePoint  NUMBER;
    nuvOldWPOINT  NUMBER;
    nuvOldRPOINT  NUMBER;
    nuvOldEPOINT  NUMBER;
    nuvNewRPOINT  NUMBER;
    nuvNewEPOINT  NUMBER;
    nuvPOINT      NUMBER;

    vch_PLANT_CD    CHAR(4)   ;
    vch_OUT_DEV_ID  CHAR(7)   ;
    vch_SP_DATA_ID  CHAR(2)   ;
    vch_STN_CD      CHAR(4)   ;
    vch_SP_KEY      CHAR(18)  ;
    vnu_DATA_SIZE   NUMBER    ;
    vlg_ORD_DATA    LONG      ;
    vch_REG_DT      DATE      ;
    vch_PROD_DT     CHAR(8)   ;
    vch_STN_SEQ     CHAR(4)   ;
    vch_SUB_SEQ     CHAR(4)   ;

    CURSOR cur_INCR1 IS
        SELECT WRITE_POINT, DATA_CNT
          FROM C_SPOOL_CTRL_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
       FOR UPDATE;

    CURSOR cur_INCR2 IS
        SELECT READ_POINT, EXE_POINT
          FROM C_SPOOL_READ_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
       FOR UPDATE;

    vrec_INCR1     cur_INCR1%ROWTYPE ;
    vrec_INCR2     cur_INCR2%ROWTYPE ;

    e_ARGS_BAD          EXCEPTION;
    e_SPOOL_FULL        EXCEPTION;
    e_NO_SPOOL          EXCEPTION;
    e_NO_POINT          EXCEPTION;
    e_NO_DATA           EXCEPTION;
    e_NO_CHANGE         EXCEPTION;
    e_KeepGoing         EXCEPTION;

  BEGIN

    pnu_RETURN := 0;
    pch_ERRMESG:= '';

    LOOP
      vnu_STEPNUM   := 10;
      IF NOT cur_INCR1%ISOPEN THEN
        OPEN cur_INCR1;
      END IF;
      IF NOT cur_INCR2%ISOPEN THEN
        OPEN cur_INCR2;
      END IF;

      vnu_STEPNUM   := 11;
      FETCH cur_INCR1 INTO vrec_INCR1;
      FETCH cur_INCR2 INTO vrec_INCR2;

      vnu_STEPNUM   := 12;
      IF cur_INCR1%NOTFOUND THEN
        RAISE e_NO_SPOOL;
      END IF;
      IF cur_INCR2%NOTFOUND THEN
        RAISE e_NO_SPOOL;
      END IF;

      vnu_WPOINT := vrec_INCR1.WRITE_POINT;
      vnu_RPOINT := vrec_INCR2.READ_POINT;
      vnu_EPOINT := vrec_INCR2.EXE_POINT;
      vnu_SIZE   := vrec_INCR1.DATA_CNT;

      IF vnu_SIZE = pnu_SIZE THEN
        RAISE e_NO_CHANGE;
      END IF;

      EXIT;

    END LOOP;

    IF cur_INCR1%ISOPEN THEN
      CLOSE cur_INCR1;
    END IF;

    IF cur_INCR2%ISOPEN THEN
      CLOSE cur_INCR2;
    END IF;

    --//*************************************************************************
    --// Insert Spool Data to Temp Table
    --//*************************************************************************
    vnu_STEPNUM   := 20;
    DELETE FROM C_SPOOL_TEMP_SP
     WHERE PLANT_CD     = pch_PLANT
       AND OUT_DEV_ID   = pch_TNAME;

    nuvRepeat     :=0;
    nuvReadPoint  := vnu_WPOINT;
    nuvOldRPOINT  := 0;
    nuvOldEPOINT  := 0;

    --// Decrease spool old read pointer
    nuvTEMPPOINT := vnu_RPOINT-1;
    IF nuvTEMPPOINT < 1  THEN
       nuvTEMPPOINT := nuvTEMPPOINT + vnu_SIZE;
    END IF;
    vnu_RPOINT := nuvTEMPPOINT;

    --// Decrease spool old exe pointer
    nuvTEMPPOINT := vnu_EPOINT-1;
    IF nuvTEMPPOINT < 1  THEN
       nuvTEMPPOINT := nuvTEMPPOINT + vnu_SIZE;
    END IF;
    vnu_EPOINT := nuvTEMPPOINT;

    LOOP
        --// Increase nuvRepeat
        vnu_STEPNUM   := 21;
        nuvRepeat  := nuvRepeat + 1;

        --// Decrease spool read pointer
        nuvTEMPPOINT := nuvReadPoint - 1 ;
        IF  nuvTEMPPOINT < 1  THEN
             nuvTEMPPOINT := nuvTEMPPOINT + vnu_SIZE;
        END IF;
        nuvReadPoint := nuvTEMPPOINT;

        vnu_STEPNUM   := 22;
        --// Check exit condition
        IF vnu_SIZE > pnu_SIZE THEN
            -- Decrease case
            IF nuvRepeat > pnu_SIZE THEN
                nuvRestoreEnd := nuvRepeat - 1;
                EXIT;
            END IF;
        ELSE
            -- Increase case
            IF nuvRepeat > vnu_SIZE THEN
                nuvRestoreEnd := nuvRepeat - 1;
                EXIT;
            END IF;
        END IF;

        --//Compare pointer and save old exe pointer
        vnu_STEPNUM   := 23;
        IF nuvReadPoint = vnu_RPOINT THEN
            nuvOldRPOINT := nuvRepeat;
        END IF;

        --//Compare pointer and save old read pointer
        vnu_STEPNUM   := 24;
        IF nuvReadPoint = vnu_EPOINT THEN
            nuvOldEPOINT := nuvRepeat;
        END IF;

        --//Initialize variables
        vnu_STEPNUM     := 25;
        vch_PLANT_CD    := ' ';
        vch_OUT_DEV_ID  := ' ';
        vch_STN_CD      := ' ';
        vch_SP_KEY      := ' ';
        vnu_DATA_SIZE   := 0;
        vlg_ORD_DATA    := ' ';
        vch_REG_DT      := SYSDATE;
        vch_PROD_DT     := ' ';
        vch_STN_SEQ     := ' ';
        vch_SUB_SEQ     := ' ';

        --// Moving C_SPOOL_TEMP_SP
        vnu_STEPNUM   := 26;
        SELECT PLANT_CD , OUT_DEV_ID, SP_DATA_ID, STN_CD  , SP_KEY    ,
               DATA_SIZE, ORD_DATA  , REG_DT  , PROD_DT   , STN_SEQ   , SUB_SEQ
          INTO vch_PLANT_CD , vch_OUT_DEV_ID, vch_SP_DATA_ID, vch_STN_CD, vch_SP_KEY ,
               vnu_DATA_SIZE, vlg_ORD_DATA  , vch_REG_DT, vch_PROD_DT   , vch_STN_SEQ, vch_SUB_SEQ
          FROM C_SPOOL_DATA_SP
         WHERE PLANT_CD     = pch_PLANT
           AND OUT_DEV_ID   = pch_TNAME
           AND SP_POINT  = nuvReadPoint;

        INSERT INTO C_SPOOL_TEMP_SP
             ( PLANT_CD , OUT_DEV_ID, SP_POINT, SP_DATA_ID, STN_CD, SP_KEY ,
               DATA_SIZE, ORD_DATA  , REG_DT  , PROD_DT   , STN_SEQ, SUB_SEQ)
        VALUES
             ( vch_PLANT_CD , vch_OUT_DEV_ID, nuvRepeat , vch_SP_DATA_ID, vch_STN_CD  , vch_SP_KEY ,
               vnu_DATA_SIZE, vlg_ORD_DATA  , vch_REG_DT, vch_PROD_DT   , vch_STN_SEQ , vch_SUB_SEQ);
        --DBMS_OUTPUT.PUT_LINE ('>nuvReadPoint:' || nuvReadPoint ) ;
        --DBMS_OUTPUT.PUT_LINE ('>vnu_RPOINT:' || vnu_RPOINT ) ;
        --DBMS_OUTPUT.PUT_LINE ('>vnu_EPOINT:' || vnu_EPOINT ) ;
        --// Compare pointer and save old read pointer

    END LOOP;--//end of LOOP(Insert Temp)

    --// Check the truncation of read/write point
    IF nuvOldRPOINT = 0 THEN
        nuvOldRPOINT := nuvRestoreEnd;
        IF pch_ENFORCE != 'Y' THEN
          vvc_DETAIL := 'NO READ DATA EXITS';
          RAISE e_SPOOL_FULL;
        END IF;
    END IF;
    IF nuvOldEPOINT = 0 THEN
        nuvOldEPOINT := nuvRestoreEnd;
        IF pch_ENFORCE != 'Y' THEN
          vvc_DETAIL := 'NO EXE DATA EXITS';
          RAISE e_SPOOL_FULL;
        END IF;
    END IF;

    --DBMS_OUTPUT.PUT_LINE ('=>nuvOldRPOINT:' || nuvOldRPOINT ) ;
    --DBMS_OUTPUT.PUT_LINE ('=>nuvOldEPOINT:' || nuvOldEPOINT ) ;
    --DBMS_OUTPUT.PUT_LINE ('=>nuvRestoreEnd:' || nuvRestoreEnd ) ;
    --//*************************************************************************
    --//Increase or decrease real spool capacity
    --//*************************************************************************
    vnu_STEPNUM   := 30;

    IF vnu_SIZE > pnu_SIZE THEN --// Decrease
        --// Skip new record insert
        GOTO RestoreData;
    ELSE --// Increase
        --// Set the count of new total record
        nuvInsertCount := pnu_SIZE;
        --// Set the point of start to insert
        nuvPOINT       := vnu_SIZE + 1;

    END IF;

    nuvRepeat := 1;
    LOOP
        --//Insert real spool record
        vnu_STEPNUM   := 31;

        BEGIN
            INSERT INTO C_SPOOL_DATA_SP(
                PLANT_CD    , OUT_DEV_ID, SP_POINT  , SP_DATA_ID, STN_CD, SP_KEY    ,
                DATA_SIZE    , ORD_DATA  , REG_DT    , PROD_DT    , STN_SEQ, SUB_SEQ    )
            VALUES(
                pch_PLANT   , pch_TNAME , nuvPOINT  ,        ' ',        ' ', ' ',
                           0,        ' ', SYSDATE,        ' ',        ' ', ' ');
        EXCEPTION
          WHEN DUP_VAL_ON_INDEX THEN
               NULL;
        END;

        --// Exit check and Increase counter
        vnu_STEPNUM   := 33;

        IF nuvPOINT >= nuvInsertCount THEN
             EXIT;
        ELSE
             nuvPOINT := nuvPOINT + 1;
        END IF;

    END LOOP;

<<RestoreData>>

    --//*************************************************************************
    --// Restore data from temp spool table to real spool table
    --//*************************************************************************
    vnu_STEPNUM   := 40;

    nuvWritePoint := pnu_SIZE;
    nuvReadPoint  := 0;

    LOOP
        --// Increase spool read pointer
        vnu_STEPNUM   := 41;
        --// increase read pointer
        nuvTEMPPOINT := nuvReadPoint + 1 ;
        IF  nuvTEMPPOINT > vnu_SIZE  THEN
             --// spool pointer exeeded the count of max spool
             nuvTEMPPOINT := nuvTEMPPOINT - vnu_SIZE;
        END IF;
        nuvReadPoint := nuvTEMPPOINT;

        --// Compare pointer and save old read pointer
        vnu_STEPNUM   := 42;
        IF nuvReadPoint = nuvOldEPOINT THEN
            nuvNewEPOINT := nuvWritePoint;
        END IF;
        IF nuvReadPoint = nuvOldRPOINT THEN
            nuvNewRPOINT := nuvWritePoint;
        END IF;

        --// Read and write data from temp to real spool (header)
        vnu_STEPNUM   := 43;
        vch_SP_DATA_ID:= ' ';
        vch_SP_KEY    := ' ';
        vnu_DATA_SIZE := 0  ;
        vlg_ORD_DATA  := ' ';
        vch_REG_DT    := SYSDATE;
        vch_PROD_DT   := ' ';
        vch_STN_SEQ   := ' ';

        vnu_STEPNUM   := 44;
        SELECT SP_DATA_ID    ,  STN_CD, SP_KEY   , DATA_SIZE    , ORD_DATA    , REG_DT    , PROD_DT    , STN_SEQ, SUB_SEQ
          INTO vch_SP_DATA_ID, vch_STN_CD, vch_SP_KEY, vnu_DATA_SIZE, vlg_ORD_DATA, vch_REG_DT, vch_PROD_DT, vch_STN_SEQ, vch_SUB_SEQ
          FROM C_SPOOL_TEMP_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           AND SP_POINT   = nuvReadPoint;

        vnu_STEPNUM   := 45;
        UPDATE C_SPOOL_DATA_SP SET
                  SP_DATA_ID = vch_SP_DATA_ID,
                  STN_CD     = vch_STN_CD    ,
                  SP_KEY     = vch_SP_KEY    ,
                  DATA_SIZE  = vnu_DATA_SIZE ,
                  ORD_DATA   = vlg_ORD_DATA  ,
                  REG_DT     = vch_REG_DT    ,
                  PROD_DT    = vch_PROD_DT   ,
                  STN_SEQ    = vch_STN_SEQ   ,
                  SUB_SEQ    = vch_SUB_SEQ
         WHERE PLANT_CD     = pch_PLANT
           AND OUT_DEV_ID   = pch_TNAME
           AND SP_POINT     = nuvWritePoint;

        --// Check exit condition
        vnu_STEPNUM   := 46;
        IF nuvReadPoint >= nuvRestoreEnd THEN
            EXIT;
        END IF;

        --// Decrease counter
        vnu_STEPNUM   := 47;
        nuvWritePoint := nuvWritePoint - 1;

    END LOOP;--// end of LOOP()

--DBMS_OUTPUT.PUT_LINE ('+>nuvNewEPOINT:' || nuvNewEPOINT ) ;
--DBMS_OUTPUT.PUT_LINE ('+>nuvNewRPOINT:' || nuvNewRPOINT ) ;
--DBMS_OUTPUT.PUT_LINE ('+>nuvWritePoint:' || nuvWritePoint ) ;

    --//*************************************************************************
    --// Update data of useless to null
    --//*************************************************************************
    vnu_STEPNUM   := 50;

    IF nuvWritePoint > 1 THEN
        LOOP
            --/ Decrease counter
            vnu_STEPNUM   := 51;
            nuvWritePoint := nuvWritePoint - 1;

            --// Update to null
            vnu_STEPNUM   := 52;
            UPDATE C_SPOOL_DATA_SP SET
                  SP_DATA_ID = ' ',
                  STN_CD     = ' ',
                  SP_KEY     = ' ',
                  DATA_SIZE  = 0,
                  ORD_DATA   = ' ',
                  REG_DT     = SYSDATE,
                  PROD_DT    = ' ',
                  SUB_SEQ    = ' ',
                  STN_SEQ    = ' '
            WHERE PLANT_CD   = pch_PLANT
              AND OUT_DEV_ID  = pch_TNAME
              AND SP_POINT    = nuvWritePoint;

            --// Check exit condition
            vnu_STEPNUM   := 53;

            IF nuvWritePoint = 0 THEN
                EXIT;
            END IF;

        END LOOP; --// End of loop B

    END IF;

    --//*************************************************************************
    --// Update new information of spool table
    --//*************************************************************************
    vnu_STEPNUM   := 60;
    --// Increase Read Pooint
    nuvTEMPPOINT := nuvNewRPOINT + 1 ;
    IF  nuvTEMPPOINT > pnu_SIZE  THEN
      nuvTEMPPOINT := nuvTEMPPOINT - pnu_SIZE;
    END IF;
    nuvNewRPOINT := nuvTEMPPOINT;

    --// Increase Read Pooint
    nuvTEMPPOINT := nuvNewEPOINT + 1 ;
    IF  nuvTEMPPOINT > pnu_SIZE  THEN
      nuvTEMPPOINT := nuvTEMPPOINT - pnu_SIZE;
    END IF;
    nuvNewEPOINT := nuvTEMPPOINT;

    UPDATE C_SPOOL_CTRL_SP SET
           WRITE_POINT = 1,
           DATA_CNT    = pnu_SIZE
     WHERE PLANT_CD    = pch_PLANT
       AND OUT_DEV_ID  = pch_TNAME;

    IF SQL%ROWCOUNT <> 1 THEN
         RAISE e_NO_SPOOL;
    END IF;

    UPDATE C_SPOOL_READ_SP SET
           READ_POINT  = nuvNewRPOINT,
           EXE_POINT   = nuvNewEPOINT
     WHERE PLANT_CD    = pch_PLANT
       AND OUT_DEV_ID  = pch_TNAME;

    IF SQL%ROWCOUNT <> 1 THEN
         RAISE e_NO_SPOOL;
    END IF;

--DBMS_OUTPUT.PUT_LINE ('++>nuvNewRPOINT:' || nuvNewRPOINT ) ;
--DBMS_OUTPUT.PUT_LINE ('++>nuvNewEPOINT:' || nuvNewEPOINT ) ;

    --//*************************************************************************
    --// Delete Old Row
    --//*************************************************************************
    vnu_STEPNUM   := 70;
    IF vnu_SIZE > pnu_SIZE THEN
      DELETE FROM C_SPOOL_DATA_SP
       WHERE  PLANT_CD   = pch_PLANT
         AND OUT_DEV_ID  = pch_TNAME
         AND SP_POINT BETWEEN pnu_SIZE+1 AND vnu_SIZE;
    END IF;

    --//*************************************************************************
    --// Delete table for temporary
    --//*************************************************************************
    vnu_STEPNUM   := 80;
    DELETE FROM C_SPOOL_TEMP_SP
     WHERE PLANT_CD     = pch_PLANT
       AND OUT_DEV_ID   = pch_TNAME;

    vnu_STEPNUM   := 999;
    pnu_RETURN  := 0 ;
  RETURN;

  EXCEPTION
    WHEN e_ARGS_BAD THEN
      pnu_RETURN  := 1 ;
      pch_ERRMESG := 'MODOULE:SPL_INCREASE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_SPOOL THEN
      pnu_RETURN  := 2 ;
      pch_ERRMESG := 'MODOULE:SPL_INCREASE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_POINT THEN
      pnu_RETURN  := 3 ;
      pch_ERRMESG := 'MODOULE:SPL_INCREASE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_SPOOL_FULL THEN
      pnu_RETURN  := 4 ;
      pch_ERRMESG := 'MODOULE:SPL_INCREASE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL(' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_CHANGE THEN
      pnu_RETURN  := 0 ;
      pch_ERRMESG := 'MODOULE:SPL_INCREASE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:INPUTED SIZE IS NOT CHANGED';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_DATA THEN
      pnu_RETURN  := 100 ;
      pch_ERRMESG := 'MODOULE:SPL_INCREASE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:READING DATA IS NOTHING';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN OTHERS THEN
      pnu_RETURN  := -1 ;
      CHK_DB_CONNECT(pnu_RETURN);
      pch_ERRMESG := 'MODOULE:SPL_INCREASE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;

    IF cur_INCR1%ISOPEN THEN
      CLOSE cur_INCR1;
    END IF;

    IF cur_INCR2%ISOPEN THEN
      CLOSE cur_INCR2;
    END IF;

    ROLLBACK;
    RETURN ;

  END SPL_INCREASE;

  PROCEDURE SPL_SET_THRESHOLD
  ( pch_PLANT    IN   CHAR      ,
    pch_TNAME    IN   CHAR      ,
    pnu_THREHOLD IN  NUMBER   ,
    pnu_RETURN   OUT NUMBER   ,
    pch_ERRMESG  OUT CHAR
  )
    IS
    vnu_STEPNUM         NUMBER  := 0;
    vvc_DETAIL          VARCHAR2(300);

    vnu_FULL_CNT        NUMBER  := 0;

    CURSOR cur_THRES IS
        SELECT FULL_CNT
          FROM C_SPOOL_CTRL_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME;

    vrec_THRES     cur_THRES%ROWTYPE ;

    e_ARGS_BAD          EXCEPTION;
    e_SPOOL_FULL        EXCEPTION;
    e_NO_SPOOL          EXCEPTION;
    e_NO_POINT          EXCEPTION;
    e_NO_DATA           EXCEPTION;
    e_NO_CHANGE         EXCEPTION;
    e_KeepGoing         EXCEPTION;

  BEGIN

    pnu_RETURN := 0;
    pch_ERRMESG:= '';

    LOOP
      vnu_STEPNUM   := 10;
      IF NOT cur_THRES%ISOPEN THEN
        OPEN cur_THRES;
      END IF;

      vnu_STEPNUM   := 11;
      FETCH cur_THRES INTO vrec_THRES;

      vnu_STEPNUM   := 12;
      IF cur_THRES%NOTFOUND THEN
        RAISE e_NO_SPOOL;
      END IF;

      vnu_FULL_CNT := vrec_THRES.FULL_CNT;

      IF vnu_FULL_CNT = pnu_THREHOLD THEN
        RAISE e_NO_CHANGE;
      END IF;

      EXIT;
    END LOOP;

    IF cur_THRES%ISOPEN THEN
      CLOSE cur_THRES;
    END IF;

    vnu_STEPNUM   := 20;
    UPDATE C_SPOOL_CTRL_SP
       SET FULL_CNT = pnu_THREHOLD,
           REG_DT      = SYSDATE
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    vnu_STEPNUM   := 11;
    IF SQL%ROWCOUNT <> 1 THEN
         RAISE e_NO_SPOOL;
    END IF;

    pnu_RETURN  := 0 ;
    RETURN;

  EXCEPTION
    WHEN e_ARGS_BAD THEN
      pnu_RETURN  := 1 ;
      pch_ERRMESG := 'MODOULE:SPL_SET_THRESHOLD, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_SPOOL THEN
      pnu_RETURN  := 2 ;
      pch_ERRMESG := 'MODOULE:SPL_SET_THRESHOLD, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_POINT THEN
      pnu_RETURN  := 3 ;
      pch_ERRMESG := 'MODOULE:SPL_SET_THRESHOLD, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_SPOOL_FULL THEN
      pnu_RETURN  := 4 ;
      pch_ERRMESG := 'MODOULE:SPL_SET_THRESHOLD, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL(' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_CHANGE THEN
      pnu_RETURN  := 0 ;
      pch_ERRMESG := 'MODOULE:SPL_SET_THRESHOLD, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:INPUTED SIZE IS NOT CHANGED';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_DATA THEN
      pnu_RETURN  := 100 ;
      pch_ERRMESG := 'MODOULE:SPL_SET_THRESHOLD, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:READING DATA IS NOTHING';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN OTHERS THEN
      pnu_RETURN  := -1 ;
      CHK_DB_CONNECT(pnu_RETURN);
      pch_ERRMESG := 'MODOULE:SPL_SET_THRESHOLD, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;

    IF cur_THRES%ISOPEN THEN
      CLOSE cur_THRES;
    END IF;

    ROLLBACK;
    RETURN ;

  END SPL_SET_THRESHOLD;


  PROCEDURE SPL_DELETE
  ( pch_PLANT    IN     CHAR        ,
    pch_TNAME    IN     CHAR        ,
    pch_ENFORCE  IN  CHAR     ,
    pnu_RETURN   OUT NUMBER   ,
    pch_ERRMESG  OUT CHAR
  )
    IS
    vnu_STEPNUM         NUMBER  := 0;
    vbo_ERRCTL          BOOLEAN := FALSE;
    vbo_ERRDATA         BOOLEAN := FALSE;
    vbo_NODATA          BOOLEAN := FALSE;

    vnu_WPOINT          NUMBER  := 0;
    vnu_RPOINT          NUMBER  := 0;
    vnu_EPOINT          NUMBER  := 0;

    e_OTHER_ERROR       EXCEPTION;

  BEGIN
    pnu_RETURN  := 0 ;
    pch_ERRMESG  := '';

    vnu_STEPNUM := 10;
    BEGIN
      SELECT WRITE_POINT
        INTO vnu_WPOINT
        FROM C_SPOOL_CTRL_SP
       WHERE PLANT_CD   = pch_PLANT
         AND OUT_DEV_ID = pch_TNAME
         FOR UPDATE;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        vbo_NODATA := TRUE;
      WHEN OTHERS THEN
        CHK_DB_CONNECT(pnu_RETURN);
        pch_ERRMESG := 'MODOULE:SPL_DELETE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
        pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    END;

    BEGIN
      SELECT READ_POINT, EXE_POINT
        INTO vnu_RPOINT, vnu_EPOINT
        FROM C_SPOOL_READ_SP
       WHERE PLANT_CD   = pch_PLANT
         AND OUT_DEV_ID = pch_TNAME
         FOR UPDATE;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        vbo_NODATA := TRUE;
      WHEN OTHERS THEN
        CHK_DB_CONNECT(pnu_RETURN);
        pch_ERRMESG := 'MODOULE:SPL_DELETE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
        pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    END;

    IF pch_ENFORCE != 'Y' AND vnu_WPOINT <> vnu_EPOINT THEN
      pnu_RETURN  := 6 ;
      RETURN;
    END IF;

    vnu_STEPNUM := 20;
    DELETE FROM C_SPOOL_CTRL_SP
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    DELETE FROM C_SPOOL_READ_SP
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    vnu_STEPNUM := 30;
    DELETE FROM C_SPOOL_DATA_SP
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    pnu_RETURN  := 0 ;
    RETURN;

  EXCEPTION
    WHEN OTHERS THEN
      pnu_RETURN  := -1 ;
      CHK_DB_CONNECT(pnu_RETURN);
      pch_ERRMESG := 'MODOULE:SPL_DELETE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    RETURN ;

  END SPL_DELETE;


  PROCEDURE SPL_CLEAR
  ( pch_PLANT    IN     CHAR        ,
    pch_TNAME    IN     CHAR        ,
    pnu_RETURN   OUT NUMBER   ,
    pch_ERRMESG  OUT CHAR
  )
    IS
    vnu_STEPNUM         NUMBER  := 0;
    vvc_DETAIL          VARCHAR2(300);
    vch_TERM_ID         CHAR(20):= ' ';

    vnu_WPOINT          NUMBER  := 0;
    vnu_RPOINT          NUMBER  := 0;
    vnu_EPOINT          NUMBER  := 0;
    vnu_SIZE            NUMBER  := 0;

    vch_WPOINT          CHAR(10):= '0';
    vnu_LENDATA         NUMBER  := 0;


    vnu_TempPOINT       NUMBER  := 0;
    vnu_TempEPOINT      NUMBER  := 0;

    e_ARGS_BAD          EXCEPTION;
    e_SPOOL_FULL        EXCEPTION;
    e_NO_SPOOL          EXCEPTION;
    e_NO_POINT          EXCEPTION;

  BEGIN
    pnu_RETURN  := 0 ;
    pch_ERRMESG  := '';

    vnu_STEPNUM   := 10;
    UPDATE C_SPOOL_CTRL_SP
       SET WRITE_POINT = 1,
           REG_DT      = SYSDATE
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    vnu_STEPNUM   := 11;
    IF SQL%ROWCOUNT <> 1 THEN
         RAISE e_NO_SPOOL;
    END IF;

    UPDATE C_SPOOL_READ_SP
       SET READ_POINT  = 1,
           EXE_POINT   = 1,
           REG_DT      = SYSDATE
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    vnu_STEPNUM   := 11;
    IF SQL%ROWCOUNT <> 1 THEN
         RAISE e_NO_SPOOL;
    END IF;

    vnu_STEPNUM   := 20;
    UPDATE C_SPOOL_DATA_SP
       SET SP_DATA_ID = ' ',
           STN_CD     = ' ',
           SP_KEY     = ' ',
           PROD_DT    = ' ',
           STN_SEQ    = ' ',
           SUB_SEQ    = ' ',
           DATA_SIZE  = 0  ,
           ORD_DATA   = ' ',
           REG_DT     =SYSDATE
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    vnu_STEPNUM   := 21;
    IF SQL%ROWCOUNT < 1 THEN
         vvc_DETAIL := '';
         RAISE e_NO_POINT;
    END IF;

    vnu_STEPNUM   := 99;
    pnu_RETURN  := 0 ;
    RETURN ;

  EXCEPTION
    WHEN e_ARGS_BAD THEN
      pnu_RETURN  := 1 ;
      pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_SPOOL THEN
      pnu_RETURN  := 2 ;
      pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_POINT THEN
      pnu_RETURN  := 3 ;
      pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_SPOOL_FULL THEN
      pnu_RETURN  := 4 ;
      pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN OTHERS THEN
      pnu_RETURN  := -1 ;
      CHK_DB_CONNECT(pnu_RETURN);
      pch_ERRMESG := 'MODOULE:SPL_WRITE, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    ROLLBACK;
    RETURN ;

  END SPL_CLEAR;


  PROCEDURE SPL_CHG_POINT
  ( pch_PLANT    IN     CHAR        ,
    pch_TNAME    IN     CHAR        ,
    pnu_WPOINT   IN  NUMBER   ,
    pnu_RPOINT   IN  NUMBER   ,
    pnu_EPOINT   IN  NUMBER   ,
    pnu_RETURN   OUT NUMBER   ,
    pch_ERRMESG  OUT CHAR
  )
    IS
    vnu_STEPNUM         NUMBER  := 0;
    vvc_DETAIL          VARCHAR2(300);

    vnu_WPOINT          NUMBER  := 0;
    vnu_RPOINT          NUMBER  := 0;
    vnu_EPOINT          NUMBER  := 0;

    tnu_WPOINT          NUMBER  := 0;
    tnu_RPOINT          NUMBER  := 0;
    tnu_EPOINT          NUMBER  := 0;

    vnu_SIZE            NUMBER  := 0;

    CURSOR cur_CHGPNT1 IS
        SELECT WRITE_POINT, DATA_CNT
          FROM C_SPOOL_CTRL_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           FOR UPDATE WAIT 5;

    CURSOR cur_CHGPNT2 IS
        SELECT READ_POINT, EXE_POINT
          FROM C_SPOOL_READ_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           FOR UPDATE WAIT 5;

    vrec_CHGPNT1     cur_CHGPNT1%ROWTYPE ;
    vrec_CHGPNT2     cur_CHGPNT2%ROWTYPE ;

    e_ARGS_BAD          EXCEPTION;
    e_SPOOL_FULL        EXCEPTION;
    e_NO_SPOOL          EXCEPTION;
    e_NO_POINT          EXCEPTION;
    e_NO_DATA           EXCEPTION;

  BEGIN

    pnu_RETURN  := 0 ;
    pch_ERRMESG  := '';

  LOOP
    vnu_STEPNUM   := 10;
    IF NOT cur_CHGPNT1%ISOPEN THEN
      OPEN cur_CHGPNT1;
    END IF;
    IF NOT cur_CHGPNT2%ISOPEN THEN
      OPEN cur_CHGPNT2;
    END IF;

    vnu_STEPNUM   := 11;
    FETCH cur_CHGPNT1 INTO vrec_CHGPNT1;
    FETCH cur_CHGPNT2 INTO vrec_CHGPNT2;

    vnu_STEPNUM   := 12;
    IF cur_CHGPNT1%NOTFOUND THEN
      RAISE e_NO_SPOOL;
    END IF;
    IF cur_CHGPNT2%NOTFOUND THEN
      RAISE e_NO_POINT;
    END IF;

    vnu_WPOINT := vrec_CHGPNT1.WRITE_POINT;
    vnu_RPOINT := vrec_CHGPNT2.READ_POINT;
    vnu_EPOINT := vrec_CHGPNT2.EXE_POINT;
    vnu_SIZE   := vrec_CHGPNT1.DATA_CNT;

    IF pnu_WPOINT < 1 THEN
      tnu_WPOINT := vnu_WPOINT;
    ELSE
      tnu_WPOINT := pnu_WPOINT;
    END IF;

    IF pnu_RPOINT < 1 THEN
      tnu_RPOINT := vnu_RPOINT;
    ELSE
      tnu_RPOINT := pnu_RPOINT;
    END IF;

    IF pnu_EPOINT < 1 THEN
      tnu_EPOINT := vnu_EPOINT;
    ELSE
      tnu_EPOINT := pnu_EPOINT;
    END IF;

    vnu_STEPNUM   := 20;
    IF tnu_WPOINT >= tnu_EPOINT THEN
      vnu_STEPNUM   := 21;
      IF tnu_RPOINT < tnu_EPOINT THEN
        vvc_DETAIL := 'EXE POINT IS FASTER THEN READ POINT';
        RAISE e_ARGS_BAD;
      END IF;
      vnu_STEPNUM   := 22;
      IF tnu_RPOINT > tnu_WPOINT THEN
        vvc_DETAIL := 'READ POINT IS FASTER THEN WRITE POINT';
        RAISE e_ARGS_BAD;
      END IF;
    ELSIF tnu_RPOINT >= tnu_WPOINT THEN
      vnu_STEPNUM   := 23;
      IF tnu_EPOINT < tnu_WPOINT THEN
        vvc_DETAIL := 'EXE POINT IS FASTER THEN READ POINT';
        RAISE e_ARGS_BAD;
      END IF;
      vnu_STEPNUM   := 24;
      IF tnu_EPOINT > tnu_RPOINT THEN
        vvc_DETAIL := 'EXE POINT IS FASTER THEN READ POINT';
        RAISE e_ARGS_BAD;
      END IF;
    ELSIF tnu_EPOINT >= tnu_RPOINT THEN
      vnu_STEPNUM   := 25;
      IF tnu_WPOINT < tnu_RPOINT THEN
        vvc_DETAIL := 'READ POINT IS FASTER THEN WRITE POINT';
        RAISE e_ARGS_BAD;
      END IF;
      vnu_STEPNUM   := 26;
      IF tnu_WPOINT > tnu_EPOINT THEN
        vvc_DETAIL := 'EXE POINT IS FASTER THEN READ POINT';
        RAISE e_ARGS_BAD;
      END IF;
    END IF;

    IF tnu_WPOINT >= tnu_RPOINT THEN
      vnu_STEPNUM   := 23;
      IF tnu_RPOINT > tnu_WPOINT THEN
        vvc_DETAIL := 'READ POINT IS FASTER THEN WRITE POINT';
        RAISE e_ARGS_BAD;
      END IF;
    ELSE
      vnu_STEPNUM   := 24;
      IF tnu_RPOINT < tnu_WPOINT THEN
        vvc_DETAIL := 'READ POINT IS FASTER THEN WRITE POINT';
        RAISE e_ARGS_BAD;
      END IF;
    END IF;

    vnu_STEPNUM   := 31;
    IF pnu_EPOINT > 0 THEN
      UPDATE C_SPOOL_READ_SP
         SET EXE_POINT = tnu_EPOINT,
             REG_DT       = SYSDATE
       WHERE PLANT_CD   = pch_PLANT
         AND OUT_DEV_ID = pch_TNAME;
      IF SQL%ROWCOUNT <> 1 THEN
        vvc_DETAIL := '' || tnu_EPOINT;
        RAISE e_NO_POINT;
      END IF;
    END IF;

    vnu_STEPNUM   := 32;
    IF pnu_RPOINT > 0 THEN
      UPDATE C_SPOOL_READ_SP
         SET READ_POINT = tnu_RPOINT,
             REG_DT       = SYSDATE
       WHERE PLANT_CD   = pch_PLANT
         AND OUT_DEV_ID = pch_TNAME;
      IF SQL%ROWCOUNT <> 1 THEN
        vvc_DETAIL := '' || tnu_RPOINT;
        RAISE e_NO_POINT;
      END IF;
    END IF;

    vnu_STEPNUM   := 33;
    IF pnu_WPOINT > 0 THEN
      UPDATE C_SPOOL_CTRL_SP
         SET WRITE_POINT = tnu_WPOINT,
             REG_DT       = SYSDATE
       WHERE PLANT_CD   = pch_PLANT
         AND OUT_DEV_ID = pch_TNAME;
      IF SQL%ROWCOUNT <> 1 THEN
        vvc_DETAIL := '' || tnu_WPOINT;
        RAISE e_NO_POINT;
      END IF;
    END IF;

    IF cur_CHGPNT1%ISOPEN THEN
      CLOSE cur_CHGPNT1;
    END IF;
    IF cur_CHGPNT2%ISOPEN THEN
      CLOSE cur_CHGPNT2;
    END IF;
    vnu_STEPNUM   := 40;
    EXIT;

  END LOOP;

  pnu_RETURN  := 0 ;
  RETURN;

  EXCEPTION
    WHEN e_ARGS_BAD THEN
      ROLLBACK;
      pnu_RETURN  := 1 ;
      pch_ERRMESG := 'MODOULE:SPL_CHG_POINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_SPOOL THEN
      ROLLBACK;
      pnu_RETURN  := 2 ;
      pch_ERRMESG := 'MODOULE:SPL_CHG_POINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_POINT THEN
      ROLLBACK;
      pnu_RETURN  := 3 ;
      pch_ERRMESG := 'MODOULE:SPL_CHG_POINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_SPOOL_FULL THEN
      ROLLBACK;
      pnu_RETURN  := 4 ;
      pch_ERRMESG := 'MODOULE:SPL_CHG_POINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN OTHERS THEN
      ROLLBACK;

      IF SQLCODE = -30006 THEN
        pnu_RETURN  := -9 ;
      ELSE
        pnu_RETURN  := -1 ;
        CHK_DB_CONNECT(pnu_RETURN);
      END IF;

      pch_ERRMESG := 'MODOULE:SPL_CHG_POINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;

    IF cur_CHGPNT1%ISOPEN THEN
      CLOSE cur_CHGPNT1;
    END IF;

    IF cur_CHGPNT2%ISOPEN THEN
      CLOSE cur_CHGPNT2;
    END IF;

    RETURN ;

  END SPL_CHG_POINT;

  PROCEDURE SPL_UPT_EPOINT
  ( pch_PLANT    IN     CHAR        ,
    pch_TNAME    IN     CHAR        ,
    pch_KEY      IN  CHAR     ,
    pnu_EPOINT   OUT NUMBER   ,
    pnu_RETURN   OUT NUMBER   ,
    pch_ERRMESG  OUT CHAR
  )
    IS
    vnu_STEPNUM         NUMBER  := 0;
    vvc_DETAIL          VARCHAR2(300);

    vnu_WPOINT          NUMBER  := 0;
    vnu_RPOINT          NUMBER  := 0;
    vnu_EPOINT          NUMBER  := 0;
    vnu_SIZE            NUMBER  := 0;

    vnu_POINT1          NUMBER  := 0;
    vnu_POINT2          NUMBER  := 0;
    vnu_SPOINT          NUMBER  := 0;
    vbo_FIND            BOOLEAN := FALSE;
    vnu_TempRPOINT      NUMBER  := 0;
    vnu_TempPOINT       NUMBER  := 0;

    CURSOR cur_CTRLU IS
        SELECT WRITE_POINT, DATA_CNT
          FROM C_SPOOL_CTRL_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME;

    CURSOR cur_UPTE1 IS
        SELECT READ_POINT, EXE_POINT
          FROM C_SPOOL_READ_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           FOR UPDATE WAIT 5;

    CURSOR cur_UPTE2 IS
        SELECT SP_POINT, SP_DATA_ID, SP_KEY
          FROM C_SPOOL_DATA_SP
         WHERE PLANT_CD   = pch_PLANT
           AND OUT_DEV_ID = pch_TNAME
           AND SP_POINT   BETWEEN vnu_POINT1 AND vnu_POINT2
         ORDER BY SP_POINT ASC;

    vrec_CTRLU     cur_CTRLU%ROWTYPE ;
    vrec_UPTE1     cur_UPTE1%ROWTYPE ;
    vrec_UPTE2     cur_UPTE2%ROWTYPE ;

    e_ARGS_BAD          EXCEPTION;
    e_SPOOL_FULL        EXCEPTION;
    e_NO_SPOOL          EXCEPTION;
    e_NO_POINT          EXCEPTION;
    e_NO_DATA           EXCEPTION;

  BEGIN

  pnu_EPOINT  := 0;
  pnu_RETURN  := 0;
  pch_ERRMESG := '';

  vnu_STEPNUM   := 10;

  LOOP

    IF NOT cur_CTRLU%ISOPEN THEN
      OPEN cur_CTRLU;
    END IF;

    vnu_STEPNUM   := 11;
    FETCH cur_CTRLU INTO vrec_CTRLU;

    vnu_STEPNUM   := 12;
    IF cur_CTRLU%NOTFOUND THEN
      RAISE e_NO_SPOOL;
    END IF;

    vnu_STEPNUM   := 13;
    vnu_WPOINT := vrec_CTRLU.WRITE_POINT;
    vnu_SIZE   := vrec_CTRLU.DATA_CNT;

    vnu_STEPNUM   := 14;
    IF cur_CTRLU%ISOPEN THEN
      CLOSE cur_CTRLU;
    END IF;

    vnu_STEPNUM   := 15;
    IF NOT cur_UPTE1%ISOPEN THEN
      OPEN cur_UPTE1;
    END IF;

    vnu_STEPNUM   := 16;
    FETCH cur_UPTE1 INTO vrec_UPTE1;

    vnu_STEPNUM   := 17;
    IF cur_UPTE1%NOTFOUND THEN
      RAISE e_NO_POINT;
    END IF;

    vnu_RPOINT := vrec_UPTE1.READ_POINT;
    vnu_EPOINT := vrec_UPTE1.EXE_POINT;

    vnu_STEPNUM   := 20;
    IF vnu_EPOINT = vnu_RPOINT THEN
      RAISE e_NO_DATA;

    ELSIF vbo_FIND != TRUE AND vnu_EPOINT < vnu_RPOINT THEN

      vnu_POINT1 := vnu_EPOINT;
      vnu_POINT2 := vnu_RPOINT-1;

      LOOP

        IF NOT cur_UPTE2%ISOPEN THEN
             OPEN cur_UPTE2;
        END IF;

        vnu_STEPNUM   := 21;
        FETCH cur_UPTE2 INTO vrec_UPTE2;

        vnu_STEPNUM   := 22;
        IF cur_UPTE2%NOTFOUND THEN
          vvc_DETAIL := '' || vnu_EPOINT;
          RAISE e_NO_POINT;
        END IF;

        IF (SUBSTR(vrec_UPTE2.SP_DATA_ID, 1, 1) = 'S') AND
           pch_KEY = vrec_UPTE2.SP_KEY THEN
          vbo_FIND   := TRUE;
          vnu_EPOINT := vrec_UPTE2.SP_POINT;
          EXIT;
        END IF;

      END LOOP;

      IF cur_UPTE2%ISOPEN THEN
        CLOSE cur_UPTE2;
      END IF;

    ELSIF vbo_FIND != TRUE AND vnu_EPOINT > vnu_RPOINT THEN
      LOOP
        vnu_POINT1 := vnu_EPOINT;
        vnu_POINT2 := vnu_SIZE;

        LOOP

          IF NOT cur_UPTE2%ISOPEN THEN
               OPEN cur_UPTE2;
          END IF;

          vnu_STEPNUM   := 21;
          FETCH cur_UPTE2 INTO vrec_UPTE2;

          vnu_STEPNUM   := 22;
          IF cur_UPTE2%NOTFOUND THEN
            EXIT;
          END IF;

          IF (SUBSTR(vrec_UPTE2.SP_DATA_ID, 1, 1) = 'S') AND
             pch_KEY = vrec_UPTE2.SP_KEY THEN
            vbo_FIND   := TRUE;
            vnu_EPOINT := vrec_UPTE2.SP_POINT;
            EXIT;
          END IF;

        END LOOP;

        IF cur_UPTE2%ISOPEN THEN
          CLOSE cur_UPTE2;
        END IF;

        IF vbo_FIND THEN
          EXIT;
        END IF;

        vnu_POINT1 := 1;
        vnu_POINT2 := vnu_RPOINT-1;

        LOOP

          IF NOT cur_UPTE2%ISOPEN THEN
               OPEN cur_UPTE2;
          END IF;

          vnu_STEPNUM   := 21;
          FETCH cur_UPTE2 INTO vrec_UPTE2;

          vnu_STEPNUM   := 22;
          IF cur_UPTE2%NOTFOUND THEN
            EXIT;
          END IF;

          IF (SUBSTR(vrec_UPTE2.SP_DATA_ID, 1, 1) = 'S') AND
             pch_KEY = vrec_UPTE2.SP_KEY THEN
            vbo_FIND   := TRUE;
            vnu_EPOINT := vrec_UPTE2.SP_POINT;
            EXIT;
          END IF;

        END LOOP;

        EXIT;

      END LOOP;

    END IF;

    IF vbo_FIND != TRUE THEN
        vvc_DETAIL := pch_KEY;
        RAISE e_NO_POINT;
    END IF;

    vnu_STEPNUM   := 30;
    vnu_TempPOINT := vnu_EPOINT + 1 ;

    IF  vnu_TempPOINT > vnu_SIZE  THEN
         vnu_TempPOINT := vnu_TempPOINT - vnu_SIZE;
    END IF;

    vnu_STEPNUM   := 31;
    UPDATE C_SPOOL_READ_SP
       SET EXE_POINT = vnu_TempPOINT,
           REG_DT       = SYSDATE
     WHERE PLANT_CD   = pch_PLANT
       AND OUT_DEV_ID = pch_TNAME;

    vnu_STEPNUM   := 32;
    IF SQL%ROWCOUNT <> 1 THEN
      vvc_DETAIL := '' || vnu_EPOINT;
      RAISE e_NO_POINT;
    END IF;

    vnu_STEPNUM   := 40;
    pnu_EPOINT := vnu_EPOINT;

    EXIT;

  END LOOP;

  IF cur_UPTE1%ISOPEN THEN
    CLOSE cur_UPTE1;
  END IF;

  vnu_STEPNUM   := 100;
  pnu_RETURN  := 0 ;
  RETURN;

  EXCEPTION
    WHEN e_ARGS_BAD THEN
      ROLLBACK;
      pnu_RETURN  := 1 ;
      pch_ERRMESG := 'MODOULE:SPL_UPT_EPOINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_SPOOL THEN
      ROLLBACK;
      pnu_RETURN  := 2 ;
      pch_ERRMESG := 'MODOULE:SPL_UPT_EPOINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_POINT THEN
      ROLLBACK;
      pnu_RETURN  := 3 ;
      pch_ERRMESG := 'MODOULE:SPL_UPT_EPOINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_SPOOL_FULL THEN
      ROLLBACK;
      pnu_RETURN  := 4 ;
      pch_ERRMESG := 'MODOULE:SPL_UPT_EPOINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN e_NO_DATA THEN
      ROLLBACK;
      pnu_RETURN  := 100 ;
      pch_ERRMESG := 'MODOULE:SPL_UPT_EPOINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:MATCHING DATA IS NOTHING(KEY:' || vvc_DETAIL || ')';
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
    WHEN OTHERS THEN
      ROLLBACK;

      IF SQLCODE = -30006 THEN
        pnu_RETURN  := -9 ;
      ELSE
        pnu_RETURN  := -1 ;
        CHK_DB_CONNECT(pnu_RETURN);
      END IF;

      pch_ERRMESG := 'MODOULE:SPL_UPT_EPOINT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;

      IF cur_UPTE2%ISOPEN THEN
        CLOSE cur_UPTE2;
      END IF;

      IF cur_CTRLU%ISOPEN THEN
        CLOSE cur_CTRLU;
      END IF;

      IF cur_UPTE1%ISOPEN THEN
        CLOSE cur_UPTE1;
      END IF;

    RETURN ;

  END SPL_UPT_EPOINT;

    PROCEDURE SPL_WAIT
    (
        pch_PLANT    IN     CHAR        ,
        pch_TNAME    IN     CHAR        ,
        pch_TIMEOUT  IN  NUMBER   ,
        pnu_WPOINT   OUT NUMBER   ,
        pnu_RETURN   OUT NUMBER   ,
        pch_ERRMESG  OUT CHAR
    )
    IS
        vnu_STEPNUM         NUMBER  := 0;
        vvc_DETAIL          VARCHAR2(300);

        vch_TERM_ID         CHAR(20):= ' ';

        vnu_WPOINT          NUMBER  := 0;
        vnu_RPOINT          NUMBER  := 0;
        vnu_EPOINT          NUMBER  := 0;
        vnu_SIZE            NUMBER  := 0;

        vnu_STATUS          NUMBER  := 0;
        vch_WPOINT          CHAR(10):= '0';
        log_txt             varchar2(4000);

        CURSOR cur_WAIT IS
        SELECT A.WRITE_POINT, B.READ_POINT, B.EXE_POINT, A.DATA_CNT
          FROM C_SPOOL_CTRL_SP A, C_SPOOL_READ_SP B
         WHERE A.PLANT_CD   = pch_PLANT
           AND A.OUT_DEV_ID = pch_TNAME
           AND A.PLANT_CD   = B.PLANT_CD
           AND A.OUT_DEV_ID = B.OUT_DEV_ID;

        vrec_WAIT           cur_WAIT%ROWTYPE ;
        e_ARGS_BAD          EXCEPTION;
        e_SPOOL_FULL        EXCEPTION;
        e_NO_SPOOL          EXCEPTION;
        e_NO_POINT          EXCEPTION;
        e_NO_DATA           EXCEPTION;

    BEGIN

        pnu_WPOINT      := 0;
        pnu_RETURN      := 0;
        pch_ERRMESG     := '';
        vnu_STEPNUM     := 10;
        --log_txt := 'pch_PLANT : ' || pch_PLANT || '::::' || 'pch_TNAME:' || pch_TNAME;
        --PKG_COM_LOGMSG.PRC_SND_LOG('E','SPOOL_TEST','STEP1 ',log_txt,'SPOOL_PKG','P');

        LOOP
            IF NOT cur_WAIT%ISOPEN THEN
                OPEN cur_WAIT;
            END IF;

            vnu_STEPNUM   := 11;
            FETCH cur_WAIT INTO vrec_WAIT;
            -- log_txt := 'vrec_WAIT:' || vrec_WAIT;
            -- --PKG_COM_LOGMSG.PRC_SND_LOG('E','SPOOL_TEST','STEP2 ',log_txt,'SPOOL_PKG','P');

            vnu_STEPNUM   := 12;
            IF cur_WAIT%NOTFOUND THEN
                RAISE e_NO_SPOOL;
            END IF;

            vnu_WPOINT := vrec_WAIT.WRITE_POINT;
            vnu_RPOINT := vrec_WAIT.READ_POINT;
            vnu_EPOINT := vrec_WAIT.EXE_POINT;
            vnu_SIZE   := vrec_WAIT.DATA_CNT;
            --log_txt := 'vnu_WPOINT,vnu_RPOINT,vnu_EPOINT,vnu_SIZE' || vnu_WPOINT || ':::' || vnu_RPOINT || ':::' || vnu_EPOINT || ':::' || vnu_SIZE;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','SPOOL_TEST','STEP2 ',log_txt,'SPOOL_PKG','P');

            vnu_STEPNUM   := 20;
            IF vnu_WPOINT != vnu_RPOINT THEN
                pnu_WPOINT := vnu_WPOINT;
                pnu_RETURN := 0 ;
                RETURN;
            END IF;
            EXIT;
        END LOOP;

        vnu_STEPNUM   := 30;
        vch_TERM_ID := pch_PLANT || pch_TNAME;
        --DBMS_ALERT.WAITONE( vch_TERM_ID, vch_WPOINT, vnu_STATUS, pch_TIMEOUT ) ;

        --log_txt := 'vch_TERM_ID : ' || vch_TERM_ID;
        --PKG_COM_LOGMSG.PRC_SND_LOG('E','SPOOL_TEST','STEP3 ',log_txt,'SPOOL_PKG','P');

        IF vnu_WPOINT = vnu_RPOINT THEN
            pnu_RETURN  := 5 ;
            RETURN;
        END IF;

        vnu_STEPNUM   := 40;
        pnu_WPOINT :=TO_NUMBER(vnu_WPOINT);


        IF pnu_WPOINT = 0 THEN
           RAISE e_NO_DATA;
        END IF;

        pnu_RETURN  := 0;
        RETURN;

    EXCEPTION
        WHEN e_ARGS_BAD THEN
            pnu_RETURN  := 1 ;
            pch_ERRMESG := 'MODOULE:SPL_WAIT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:ARGUMENT IS BAD(:' || vvc_DETAIL || ')';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WAIT','e_ARGS_BAD',pch_ERRMESG,'EXCEPTION','P');
        WHEN e_NO_SPOOL THEN
            pnu_RETURN  := 2 ;
            pch_ERRMESG := 'MODOULE:SPL_WAIT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS NOTHING';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WAIT','e_NO_SPOOL',pch_ERRMESG,'EXCEPTION','P');
        WHEN e_NO_POINT THEN
            pnu_RETURN  := 3 ;
            pch_ERRMESG := 'MODOULE:SPL_WAIT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL POINT IS NOTHING(POINT:' || vvc_DETAIL || ')';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WAIT','e_NO_POINT',pch_ERRMESG,'EXCEPTION','P');
        WHEN e_SPOOL_FULL THEN
            pnu_RETURN  := 4 ;
            pch_ERRMESG := 'MODOULE:SPL_WAIT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:SPOOL IS FULL';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;

            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WAIT','e_SPOOL_FULL',pch_ERRMESG,'EXCEPTION','P');
        WHEN e_NO_DATA THEN
            pnu_RETURN  := 100 ;
            pch_ERRMESG := 'MODOULE:SPL_WAIT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', MESG:MATCHING DATA IS NOTHING(KEY:' || vvc_DETAIL || ')';
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;

            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WAIT','e_NO_DATA',pch_ERRMESG,'EXCEPTION','P');
        WHEN OTHERS THEN
            pnu_RETURN  := -1 ;
            CHK_DB_CONNECT(pnu_RETURN);
            pch_ERRMESG := 'MODOULE:SPL_WAIT, DEVICE:' || pch_TNAME || ', STEP:' || TO_CHAR(vnu_STEPNUM) || ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
            pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
            --PKG_COM_LOGMSG.PRC_SND_LOG('E','PKG_SPOOLT.SPL_WAIT','OTHERS',pch_ERRMESG,'EXCEPTION','P');


        IF cur_WAIT%ISOPEN THEN
            CLOSE cur_WAIT;
        END IF;

        RETURN ;

    END SPL_WAIT;


  PROCEDURE CHK_DB_CONNECT
  (
    pnu_RETURN   IN OUT NUMBER
  )
    IS
    vnu_STEPNUM         NUMBER  := 0;

    BEGIN
        IF SQLCODE = -3113  OR SQLCODE = -3114  OR SQLCODE = -12541 OR
            SQLCODE = -1033  OR SQLCODE = -1089  OR SQLCODE = -1090  OR
            SQLCODE = -1034  OR SQLCODE = -12224 OR SQLCODE = -60    OR
            SQLCODE = -1041  OR
            SQLCODE = -12569 OR SQLCODE = -12570 OR SQLCODE = -12571 OR
            SQLCODE = -20000 THEN
            pnu_RETURN  := -2 ;
        ELSE
            pnu_RETURN  := pnu_RETURN ;
        END IF;

        RETURN;

    END CHK_DB_CONNECT;

  PROCEDURE REGISTER
  (
    pch_TermName   IN    CHAR,      -- Terminal Name
    pnu_RETURN     OUT   NUMBER,    -- Result
    pch_ERRMESG    OUT   CHAR       -- SQLMESG
  )
    IS
    vnu_STEPNUM         NUMBER  := 0    ;
    vch_TERM_ID         CHAR(20):= ' ';

    BEGIN

        DBMS_ALERT.REGISTER( pch_TermName ) ;
        pnu_RETURN  := 0 ;
        RETURN ;

    EXCEPTION
        WHEN OTHERS THEN
          CHK_DB_CONNECT(pnu_RETURN);
          pch_ERRMESG := 'STEP:' || TO_CHAR(vnu_STEPNUM) || ', MODULE:PMSENYOU' ||  ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
          pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
          RETURN ;

    END REGISTER;

  PROCEDURE SIGNAL
  ( pch_TermName   IN    CHAR    , -- Terminal Name
    pch_Message    IN    CHAR    , -- Message
    pnu_RETURN     OUT    NUMBER    , -- Result
    pch_ERRMESG    OUT    CHAR      -- SQLMESG
  )
    IS
    vnu_STEPNUM         NUMBER  := 0    ;         -- SOURCE CODE LINE No. (DEBUG#?)

    vch_Message    CHAR(256); -- Terminal Name

  BEGIN

    vch_Message := SUBSTR(pch_Message, 1, 256);

    DBMS_ALERT.SIGNAL( pch_TermName, vch_Message ) ;

    pnu_RETURN  := 0 ;
    RETURN ;

  EXCEPTION
    WHEN OTHERS THEN
      IF SQLCODE = -3113  OR SQLCODE = -3114  OR SQLCODE = -12541 OR
        SQLCODE = -1033  OR SQLCODE = -1089  OR SQLCODE = -1090  OR
        SQLCODE = -1034  OR SQLCODE = -12224 OR SQLCODE = -60    OR
        SQLCODE = -1041  OR
        SQLCODE = -12569 OR SQLCODE = -12570 OR SQLCODE = -12571 OR
        SQLCODE = -20000 THEN
        pnu_RETURN  := 98 ;
      ELSE
        pnu_RETURN  := 1 ;
      END IF;
      pch_ERRMESG := 'STEP : ' || TO_CHAR(vnu_STEPNUM) || ', MODULE:PMSENYOU' ||  ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
      RETURN ;

  END SIGNAL;

  PROCEDURE WAITONE
  ( pch_TermName   IN    CHAR    , -- Terminal Name
    pch_Message      OUT    CHAR,        -- Message
    pnu_Status      OUT    INTEGER,    -- Status
    pnu_TimeOut      IN    NUMBER,        -- Time Out
    pnu_RETURN     OUT    NUMBER    , -- Result
    pch_ERRMESG    OUT    CHAR      -- SQLMESG
  )
    IS
    vnu_STEPNUM         NUMBER  := 0    ;         -- SOURCE CODE LINE No. (DEBUG#?)

    vch_Message    CHAR(256); -- Terminal Name

  BEGIN
    pnu_Status :=0;
    DBMS_ALERT.WAITONE( pch_TermName, vch_Message, pnu_Status, pnu_TimeOut ) ;

    pch_Message :=SUBSTR(vch_Message, 1, 256);

    pnu_RETURN  := 0 ;
    RETURN ;

  EXCEPTION
    WHEN OTHERS THEN
      IF SQLCODE = -3113  OR SQLCODE = -3114  OR SQLCODE = -12541 OR
        SQLCODE = -1033  OR SQLCODE = -1089  OR SQLCODE = -1090  OR
        SQLCODE = -1034  OR SQLCODE = -12224 OR SQLCODE = -60    OR
        SQLCODE = -1041  OR
        SQLCODE = -12569 OR SQLCODE = -12570 OR SQLCODE = -12571 OR
        SQLCODE = -20000 THEN
        pnu_RETURN  := 98 ;
      ELSE
        pnu_RETURN  := 1 ;
      END IF;
      pch_ERRMESG := 'STEP : ' || TO_CHAR(vnu_STEPNUM) || ', MODULE:PMSENYOU' ||  ', SQLERR:' || SQLCODE  || ', MESG:' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
      RETURN ;

  END WAITONE;

  PROCEDURE REMOVE
  ( pch_TermName   IN    CHAR    , -- Terminal Name
    pnu_RETURN     OUT    NUMBER    , -- Result
    pch_ERRMESG    OUT    CHAR      -- SQLMESG
  )
    IS
    vnu_STEPNUM         NUMBER  := 0    ;         -- SOURCE CODE LINE No. (DEBUG#?)

  BEGIN

    DBMS_ALERT.REMOVE( pch_TermName ) ;

    pnu_RETURN  := 0 ;
    RETURN ;

  EXCEPTION
    WHEN OTHERS THEN
      IF SQLCODE = -3113  OR SQLCODE = -3114  OR SQLCODE = -12541 OR
        SQLCODE = -1033  OR SQLCODE = -1089  OR SQLCODE = -1090  OR
        SQLCODE = -1034  OR SQLCODE = -12224 OR SQLCODE = -60    OR
        SQLCODE = -1041  OR
        SQLCODE = -12569 OR SQLCODE = -12570 OR SQLCODE = -12571 OR
        SQLCODE = -20000 THEN
        pnu_RETURN  := 98 ;
      ELSE
        pnu_RETURN  := -2 ;
      END IF;
      pch_ERRMESG := 'STEP : ' || TO_CHAR(vnu_STEPNUM) || ', MODULE:PMALERT, MESG : ' || SQLERRM ;
      pch_ERRMESG := SUBSTR (pch_ERRMESG, 1, 256) ;
      RETURN ;

  END REMOVE;

END;
/