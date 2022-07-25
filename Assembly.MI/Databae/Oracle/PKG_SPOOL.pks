CREATE OR REPLACE PACKAGE HMBMESV1ADM."PKG_SPOOL" 
IS
    PROCEDURE SPL_WRITE
    (
    pch_PLANT        IN           CHAR    , X
    pch_TNAME        IN           CHAR    , (out_device_id)
    pch_KIND        IN           CHAR    ,  (spool_data_id)
    pch_KEY            IN           CHAR    , (spool_key) 차량번호, 바코드번호
    pch_PROD_DT        IN           CHAR    , (plant_date) 생산일자
    pch_STN_CD        IN           CHAR    ,  (station_code) 공정코드
    pch_STN_SEQ        IN           CHAR    , (station_seq) 공정 시퀀스
    pch_SUB_SEQ        IN           CHAR    , X
    plg_SPDATA        IN           LONG    , (order_data) 지시내용
    pnu_WPOINT        OUT        NUMBER,     (spool_point)
    pnu_RETURN      OUT        NUMBER,
    pch_ERRMESG     OUT        CHAR
  );

  PROCEDURE SPL_TERMINIT
  (
    pch_PLANT       IN         CHAR,
    pch_TNAME       IN         CHAR,
    pnu_RETURN      OUT        NUMBER,
    pch_ERRMESG     OUT        CHAR
  );

  PROCEDURE SPL_READ
  (
    pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pnu_UPTEPOINT    IN         NUMBER    ,
    pnu_RPOINT       OUT        NUMBER    ,
    pch_KIND         OUT        CHAR    ,
    pch_KEY          OUT        CHAR    ,
    pch_PROD_DT      OUT        CHAR    ,
    pch_STN_CD       OUT        CHAR    ,
    pch_STN_SEQ      OUT        CHAR    ,
    pch_SUB_SEQ      OUT        CHAR    ,
    pnu_LENGTH       OUT        NUMBER    ,
    plg_SPDATA       OUT        LONG    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_RETRIEVE
  ( pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pnu_EPOINT       IN OUT     NUMBER    ,
    pch_KIND         OUT        CHAR    ,
    pch_KEY          OUT        CHAR    ,
    pch_PROD_DT      OUT        CHAR    ,
    pch_STN_CD       OUT        CHAR    ,
    pch_STN_SEQ      OUT        CHAR    ,
    pch_SUB_SEQ      OUT        CHAR    ,
    pnu_LENGTH       OUT        NUMBER    ,
    plg_SPDATA       OUT        LONG    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_ALL_RETRIEVE
  ( pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pnu_EPOINT       IN OUT     NUMBER    ,
    pch_KIND         OUT        CHAR    ,
    pch_KEY          OUT        CHAR    ,
    pch_PROD_DT      OUT        CHAR    ,
    pch_STN_CD       OUT        CHAR    ,
    pch_STN_SEQ      OUT        CHAR    ,
    pch_SUB_SEQ      OUT        CHAR    ,
    pnu_LENGTH       OUT        NUMBER    ,
    plg_SPDATA       OUT        LONG    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_CREATE
  ( pch_PLANT        IN         CHAR    , X
    pch_TNAME        IN         CHAR    , (out_device_id)
    pnu_SIZE         IN         NUMBER    , (data_count)
    pnu_THREHOLD     IN         NUMBER    , (full_count)
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_DELETE
  ( pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pch_ENFORCE      IN         CHAR    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_INCREASE
  ( pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pnu_SIZE         IN         NUMBER    ,
    pch_ENFORCE      IN         CHAR    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_SET_THRESHOLD
  ( pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pnu_THREHOLD     IN         NUMBER    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_CLEAR
  ( pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_CHG_POINT
  ( pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pnu_WPOINT       IN         NUMBER    ,
    pnu_RPOINT       IN         NUMBER    ,
    pnu_EPOINT       IN         NUMBER    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_UPT_EPOINT
  ( pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pch_KEY          IN         CHAR    ,
    pnu_EPOINT       OUT        NUMBER    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE SPL_WAIT
  ( pch_PLANT        IN         CHAR    ,
    pch_TNAME        IN         CHAR    ,
    pch_TIMEOUT      IN         NUMBER    ,
    pnu_WPOINT       OUT        NUMBER    ,
    pnu_RETURN       OUT        NUMBER    ,
    pch_ERRMESG      OUT        CHAR
  );

  PROCEDURE CHK_DB_CONNECT
  ( pnu_RETURN      IN OUT     NUMBER
  );

  PROCEDURE REGISTER
  ( pch_TermName    IN         CHAR    ,    -- Terminal Name
    pnu_RETURN      OUT        NUMBER    ,    -- Result
    pch_ERRMESG     OUT        CHAR        -- SQLMESG
  );

  PROCEDURE SIGNAL
  ( pch_TermName    IN         CHAR    ,    -- Terminal Name
    pch_Message     IN         CHAR    ,    -- Message
    pnu_RETURN      OUT        NUMBER    ,    -- Result
    pch_ERRMESG     OUT        CHAR        -- SQLMESG
  );

  PROCEDURE WAITONE
  ( pch_TermName    IN         CHAR    ,    -- Terminal Name
    pch_Message     OUT        CHAR    ,    -- Message
    pnu_Status      OUT        INTEGER    ,    -- Status
    pnu_TimeOut     IN         NUMBER    ,    -- Time Out
    pnu_RETURN      OUT        NUMBER    ,    -- Result
    pch_ERRMESG     OUT        CHAR        -- SQLMESG
  );

  PROCEDURE REMOVE
  ( pch_TermName    IN         CHAR    ,    -- Terminal Name
    pnu_RETURN      OUT        NUMBER    ,    -- Result
    pch_ERRMESG     OUT        CHAR        -- SQLMESG
  );

END;
/