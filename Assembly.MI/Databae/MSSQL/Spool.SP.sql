
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.10.21
-- Description:	Spool Initialize
-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_TERMINIT]
(
	@out_device_id	char(7),
	@data_count	int,
	@full_count	int,
	@_RETURN	int output,
	@_ERRMESG	nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	SET NOCOUNT ON;

	DECLARE @_STEPNUM	int,
			@_TERM_ID	nchar(20)

	set @_STEPNUM = 0;
	set @_TERM_ID = '';

	set @_RETURN  = 0;
	set @_ERRMESG  = '';

	BEGIN TRY

		set @_STEPNUM = 1;
		set @_TERM_ID = RTRIM(@out_device_id);
		--DBMS_ALERT.REGISTER(@_TERM_ID);

		set @_RETURN  = 0;

	END TRY

	BEGIN CATCH
      SET @_RETURN  = -1;
      --CHK_DB_CONNECT(@_RETURN);
      SET @_ERRMESG = 'MODOULE:USP_SPOOL_TERMINIT, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_NUMBER() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
      SET @_ERRMESG = substring(@_ERRMESG, 1, 256);
      --ROLLBACK;

	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.10.21
-- Description:	Create Spool
-- Usage
--	DECLARE @out_device_id	char(7),
--		@data_count	int,
--		@full_count	int,
--		@_RETURN	int ,
--		@_ERRMESG	nvarchar(256)

--	SET @out_device_id = 'ABCD';
--	SET @data_count = 100;
--	SET @full_count = 100;

--	exec USP_SPOOL_CREATE @out_device_id, @data_count, @full_count, @_RETURN output, @_ERRMESG output
--	select @_RETURN, @_ERRMESG
-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_CREATE]
(
	@out_device_id	char(7),
	@data_count	int,
	@full_count	int,
	@_RETURN	int output,
	@_ERRMESG	nvarchar(256)	output
)
AS
BEGIN
-- SET NOCOUNT ON added to prevent extra result sets from
SET NOCOUNT ON;

DECLARE @_STEPNUM	int,
		@_POINT	int

set @_STEPNUM = 0;
set @_POINT = 1;

set @_RETURN  = 0;
set @_ERRMESG  = '';

BEGIN TRY

	BEGIN TRAN;

	set @_STEPNUM = 10;
	-- insert SS_SPOOL_WRITE
	INSERT INTO SS_SPOOL_WRITE(out_device_id, write_point, data_count, full_count, alarm_count, usage_flag, reg_datetime)
	VALUES (@out_device_id, 1, @data_count, @full_count, 10, '0', getdate());

	-- insert SS_SPOOL_READ
	INSERT INTO SS_SPOOL_READ(out_device_id, read_point, exe_point, reg_datetime)
	VALUES (@out_device_id, 1, 1, getdate());

	set @_STEPNUM = 20;
	-- delete SS_SPOOL_DATA
	DELETE FROM SS_SPOOL_DATA
	WHERE out_device_id   = @out_device_id;

	set @_STEPNUM = 30;

	WHILE (@_POINT <= @data_count)
		BEGIN
			--plant_date: 생산 일자
			--station_code: 공정 코드
			--station_seq: 공정 시퀀스
			--spool_key: 차량번호(현대차), 바코드번호(모비스)
			--data_size: 지시내용의 크기
			--order_data: 지시내용
			INSERT INTO SS_SPOOL_DATA (out_device_id, spool_point, spool_data_id, plant_date, station_code,
										station_seq, spool_key, data_size, order_data, reg_datetime)
			VALUES (@out_device_id, @_POINT, '', '', '',
										'', '', 0, '', getdate());

			SET @_POINT = @_POINT + 1;
		END

		set @_RETURN  = 0;

		COMMIT TRAN;

	END TRY

	BEGIN CATCH

		ROLLBACK TRAN;

      SET @_RETURN  = -1;
      --CHK_DB_CONNECT(@_RETURN);
      SET @_ERRMESG = 'MODOULE:USP_SPOOL_CREATE, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_NUMBER() as nvarchar(256)) + ', MESG:' + ERROR_MESSAGE();
      SET @_ERRMESG = substring(@_ERRMESG, 1, 256);

	END CATCH 
END
GO



SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.10.21
-- Description:	Spool Delete
-- Usage:
--DECLARE @out_device_id	char(7),
--		@eNFORCE	char(1),
--		@_RETURN	int ,
--		@_ERRMESG	nvarchar(256)

--SET @out_device_id = 'ABCD';
--SET @eNFORCE = 'N';

--exec USP_SPOOL_DELETE @out_device_id, @eNFORCE, @_RETURN output, @_ERRMESG output
--select @_RETURN, @_ERRMESG
-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_DELETE]
(
	@out_device_id	char(7),
	@eNFORCE	char(1),
	@_RETURN	int output,
	@_ERRMESG	nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	SET NOCOUNT ON;

	DECLARE @_STEPNUM	int,
			@_ERRCTL	bit,
			@_ERRDATA	bit,
			@_NODATA	bit,
			@_WPOINT	int,
			@_RPOINT	int,
			@_EPOINT	int

	set @_STEPNUM	= 0;
	set @_ERRCTL	= 0; --false
	set @_ERRDATA	= 0; --false
	set @_NODATA	= 0; --false
	set @_WPOINT	= 0;
	set @_RPOINT	= 0;
	set @_EPOINT	= 0;

	set @_RETURN  = 0;
	set @_ERRMESG  = '';


	BEGIN TRY
		BEGIN
			BEGIN TRY
				set @_STEPNUM = 10;
				set @_WPOINT = ISNULL((SELECT write_point FROM SS_SPOOL_WRITE WHERE out_device_id = @out_device_id), -1);

				if (@_WPOINT < 0)
				BEGIN
					set @_NODATA = 1;
				END

			END TRY

			BEGIN CATCH
			  --CHK_DB_CONNECT(@_RETURN);
			  SET @_ERRMESG = 'MODOULE:USP_SPOOL_DELETE, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_NUMBER() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
			  SET @_ERRMESG = substring(@_ERRMESG, 1, 256);
			END CATCH 

			BEGIN TRY
				set @_STEPNUM = 20;
				set @_RPOINT = ISNULL((SELECT read_point FROM SS_SPOOL_READ WHERE out_device_id = @out_device_id), -1);
				set @_EPOINT = ISNULL((SELECT exe_point FROM SS_SPOOL_READ WHERE out_device_id = @out_device_id), -1);

				if (@_RPOINT < 0)
				BEGIN
					set @_NODATA = 1;
				END

			END TRY

			BEGIN CATCH
			  --CHK_DB_CONNECT(@_RETURN);
			  SET @_ERRMESG = 'MODOULE:USP_SPOOL_DELETE, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_NUMBER() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
			  SET @_ERRMESG = substring(@_ERRMESG, 1, 256);
			END CATCH 

			if (@eNFORCE != 'Y' and @_WPOINT != @_EPOINT)
			begin
			  set @_RETURN  = 6;
			  RETURN;
			end

			set @_STEPNUM = 30;
			DELETE FROM SS_SPOOL_WRITE WHERE out_device_id = @out_device_id;
			DELETE FROM SS_SPOOL_READ WHERE out_device_id = @out_device_id;
			DELETE FROM SS_SPOOL_DATA WHERE out_device_id = @out_device_id;

			set @_RETURN  = 0;
			RETURN;
		END
	END TRY

	BEGIN CATCH
		set @_RETURN = -1;
		--CHK_DB_CONNECT(@_RETURN);
		SET @_ERRMESG = 'MODOULE:USP_SPOOL_DELETE, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_NUMBER() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);
	END CATCH 
END
GO



SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.10.21
-- Description:	Spool Increase (Spool Record Count Increase in SS_SPOOL_DATA table)
-- Usage:
--DECLARE @out_device_id	char(7),
--		@sIZE	int,
--		@eNFORCE	char(1),
--		@_RETURN	int ,
--		@_ERRMESG	nvarchar(256)
--
--SET @out_device_id = 'ABCD';
--SET @sIZE = 25;
--SET @eNFORCE = 'Y';
--
--exec USP_SPOOL_INCREASE @out_device_id, @sIZE, @eNFORCE, @_RETURN output, @_ERRMESG output
--
--select @_RETURN, @_ERRMESG

-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_INCREASE]
(
	@out_device_id	char(7),
	@sIZE	int,
	@eNFORCE	char(1),
	@_RETURN	int output,
	@_ERRMESG	nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	SET NOCOUNT ON;

	DECLARE @_STEPNUM	int,

			@data_count	int,
			@read_point	int,
			@write_point	int,
			@exe_point	int,

			@_TEMPPOINT  int,
			@_Repeat     int,            -- Temporary spool point
			@_ReadPoint  int,
			@_RestoreEnd int,


			@v_out_device_id	char(7),
			@v_spool_data_id	char(2),
			@v_plant_date	char(8),
			@v_station_code	char(5),
			@v_station_seq	char(4),
			@v_spool_key	char(50),
			@v_data_size	int,
			@v_order_data	nvarchar(max),
			@v_reg_datetime	datetime,


			@_WPOINT	int,
			@_RPOINT	int,
			@_EPOINT	int,
			@_SIZE	int,

			@_OldRPOINT  int,
			@_OldEPOINT  int,

			@_POINT      int,
			@_InsertCount int,
			@_RealCount  int,

			
			@_WritePoint  int,
			@_OldWPOINT  int,

			@_NewRPOINT  int,
			@_NewEPOINT  int;

	set @_STEPNUM	= 0;


	set @_RETURN  = 0 ;
	set @_ERRMESG  = '';

	BEGIN TRY

		BEGIN TRAN;

		set @_STEPNUM	= 10;

		set @data_count = ISNULL((SELECT data_count FROM SS_SPOOL_WRITE WHERE out_device_id = @out_device_id), -1);
		set @read_point = ISNULL((SELECT read_point FROM SS_SPOOL_READ WHERE out_device_id = @out_device_id), -1);

		set @_STEPNUM	= 20;

		if (@data_count < 0 or @read_point < 0)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
					11, -- Severity,  
					2 -- Message id. -- State,  
					);
		end

		set @_STEPNUM	= 30;
		
		if (@data_count = @sIZE)
		begin
			--e_NO_CHANGE
			RAISERROR (N'INPUTED SIZE IS NOT CHANGED',  
					11, -- Severity,  
					0 -- Message id. -- State,  
					);
		end

		set @write_point = ISNULL((SELECT write_point FROM SS_SPOOL_WRITE WHERE out_device_id = @out_device_id), -1);
		set @exe_point = ISNULL((SELECT exe_point FROM SS_SPOOL_READ WHERE out_device_id = @out_device_id), -1);

		--//*************************************************************************
		--// Insert Spool Data to Temp Table
		--//*************************************************************************

		set @_STEPNUM	= 40;
		DELETE FROM SS_SPOOL_TEMP WHERE out_device_id = @out_device_id;

		set @_Repeat = 0;
		set @_ReadPoint  = @write_point;
		set @_OldRPOINT  = 0;
		set @_OldEPOINT  = 0;

		--// Decrease spool old read pointer
		set @_STEPNUM	= 50;
		set @_TEMPPOINT = @read_point-1;
		if (@_TEMPPOINT < 1)
		begin
		   set @_TEMPPOINT = @_TEMPPOINT + @data_count;
		end
		set @read_point = @_TEMPPOINT;

		--// Decrease spool old exe pointer
		set @_STEPNUM	= 60;
		set @_TEMPPOINT = @exe_point-1;
		if (@_TEMPPOINT < 1)
		begin
		   set @_TEMPPOINT = @_TEMPPOINT + @data_count;
		end
		set @exe_point = @_TEMPPOINT;

		WHILE (100 > 99)
		BEGIN
			--// Increase nuvRepeat
			set @_STEPNUM	= 70;
			set @_Repeat = @_Repeat + 1;

			--// Decrease spool read pointer
			set @_STEPNUM	= 80;
			set @_TEMPPOINT = @_ReadPoint-1;
			if (@_TEMPPOINT < 1)
			begin
			   set @_TEMPPOINT = @_TEMPPOINT + @data_count;
			end
			set @_ReadPoint = @_TEMPPOINT;

			set @_STEPNUM	= 90;
			--// Check exit condition
			if (@data_count > @sIZE)
			begin
				-- Decrease case
				if (@_Repeat > @sIZE)
				begin
					set @_RestoreEnd = @_Repeat - 1;
					break;
				end
			end
			else
			begin
				-- Increase case
				if (@_Repeat > @data_count)
				begin
					set @_RestoreEnd = @_Repeat - 1;
					break;
				end
			end

			 --//Compare pointer and save old read pointer
			set @_STEPNUM	= 100;
			if (@_ReadPoint = @read_point)
			begin
			   set @_OldRPOINT = @_Repeat;
			end

			--//Compare pointer and save old exe pointer
			set @_STEPNUM	= 110;
			if (@_ReadPoint = @exe_point)
			begin
				set @_OldEPOINT = @_Repeat;
			end

			--//Initialize variables
			set @_STEPNUM    = 120;
			set @v_out_device_id = '';
			set @v_spool_data_id = '';
			set @v_plant_date = '';
			set @v_station_code = '';
			set @v_station_seq = '';
			set @v_spool_key = '';
			set @v_data_size = 0;
			set @v_order_data = '';
			set @v_reg_datetime = getdate();

			--// Moving SS_SPOOL_TEMP

			 SELECT @v_out_device_id = out_device_id, 
					@v_spool_data_id = spool_data_id,
					@v_plant_date = plant_date,
					@v_station_code = station_code,
					@v_station_seq = station_seq,
					@v_spool_key = spool_key,
					@v_data_size = data_size,
					@v_order_data = order_data,
					@v_reg_datetime = reg_datetime
			 FROM SS_SPOOL_DATA
			 WHERE out_device_id = @out_device_id
			   AND spool_point = @_ReadPoint;

			INSERT INTO SS_SPOOL_TEMP
				 (out_device_id, spool_point, spool_data_id, plant_date, station_code, 
				   station_seq, spool_key, data_size, order_data, reg_datetime)
			VALUES
				 (@v_out_device_id, @_Repeat, @v_spool_data_id, @v_plant_date, @v_station_code, 
				   @v_station_seq, @v_spool_key, @v_data_size, @v_order_data, @v_reg_datetime);

		END

		set @_STEPNUM	= 120;

		--// Check the truncation of read/write point
		if (@_OldRPOINT = 0)
		begin
			set @_OldRPOINT = @_RestoreEnd;
			if (@eNFORCE != 'Y')
			begin
				  --e_SPOOL_FULL
				RAISERROR (N'SPOOL IS FULL(NO READ DATA EXITS)',  
					11, -- Severity,  
					4 -- Message id. -- State,  
					);
			end
		end

		set @_STEPNUM	= 130;

		if (@_OldEPOINT = 0)
		begin
			set @_OldEPOINT = @_RestoreEnd;
			if (@eNFORCE != 'Y')
			begin
				--e_SPOOL_FULL
				RAISERROR (N'SPOOL IS FULL(NO EXE DATA EXITS)',  
					11, -- Severity,  
					4 -- Message id. -- State,  
					);
			end
		end

		--//*************************************************************************
		--//Increase or decrease real spool capacity
		--//*************************************************************************
		set @_STEPNUM	= 140;

		if (@data_count > @sIZE) --// Decrease
		begin
			--// Skip new record insert
			GOTO RestoreData;
		end
		else --// Increase
		begin
			--// Set the count of new total record
			set @_InsertCount = @sIZE;
			--// Set the point of start to insert
			set @_POINT       = @data_count + 1;
		end

		set @_Repeat = 1;
		set @_STEPNUM	= 150;

		WHILE (@_POINT <= @_InsertCount)
		begin
			 INSERT INTO SS_SPOOL_DATA (out_device_id, spool_point, spool_data_id, plant_date, station_code, 
				   station_seq, spool_key, data_size, order_data, reg_datetime)
             VALUES(@out_device_id, @_POINT, '', '', '',
                    '', '', 0, '', getdate());

			set @_POINT = @_POINT + 1;
		end


		RestoreData:

		--//*************************************************************************
		--// Restore data from temp spool table to real spool table
		--//*************************************************************************
		set @_STEPNUM	= 160;

		set @_WritePoint = @sIZE;
		set @_ReadPoint  = 0;

		WHILE (@_ReadPoint < @_RestoreEnd)
		begin
			--// Increase spool read pointer
			set @_STEPNUM	= 170;

			--// increase read pointer
			set @_TEMPPOINT = @_ReadPoint + 1;
			if (@_TEMPPOINT > @sIZE)
			begin
				 --// spool pointer exeeded the count of max spool
				 set @_TEMPPOINT = @_TEMPPOINT - @sIZE;
			end
			set @_ReadPoint = @_TEMPPOINT;

			 --// Compare pointer and save old read pointer
			set @_STEPNUM	= 180;
			if (@_ReadPoint = @_OldEPOINT)
			begin
				set @_NewEPOINT = @_WritePoint;
			end

			if (@_ReadPoint = @_OldRPOINT)
			begin
				set @_NewRPOINT = @_WritePoint;
			end

			 --// Read and write data from temp to real spool (header)
			set @_STEPNUM	= 190;

			set @v_spool_data_id = '';
			set @v_plant_date = '';
			set @v_station_code = '';
			set @v_station_seq = '';
			set @v_spool_key = '';
			set @v_data_size = 0;
			set @v_order_data = '';
			set @v_reg_datetime = getdate();

			 SELECT @v_spool_data_id = spool_data_id,
					@v_plant_date = plant_date,
					@v_station_code = station_code,
					@v_station_seq = station_seq,
					@v_spool_key = spool_key,
					@v_data_size = data_size,
					@v_order_data = order_data,
					@v_reg_datetime = reg_datetime
			 FROM SS_SPOOL_TEMP
			 WHERE out_device_id = @out_device_id
			   AND spool_point = @_ReadPoint;

			set @_STEPNUM	= 200;

			UPDATE SS_SPOOL_DATA SET
                  spool_data_id = @v_spool_data_id,
                  plant_date     = @v_plant_date    ,
                  station_code  = @v_station_code ,
                  station_seq   = @v_station_seq  ,
                  spool_key     = @v_spool_key    ,
                  data_size    = @v_data_size   ,
                  order_data    = @v_order_data   ,
                  reg_datetime    = @v_reg_datetime
			 WHERE out_device_id   = @out_device_id
			   AND spool_point     = @_WritePoint;

			set @_STEPNUM	= 210;
			set @_WritePoint = @_WritePoint - 1;

		end

		--//*************************************************************************
		--// Update data of useless to null
		--//*************************************************************************
		set @_STEPNUM	= 220;

		if (@_WritePoint > 1)
		begin
			--loop
			WHILE (@_WritePoint != 0)
			BEGIN
				--/ Decrease counter
				set @_STEPNUM	= 230;
				set @_WritePoint = @_WritePoint - 1;
			
				 --// Update to null
				UPDATE SS_SPOOL_DATA SET
					  spool_data_id = '',
					  plant_date     = '',
					  station_code     = '',
					  station_seq  = '',
					  spool_key   = '',
					  data_size     = 0,
					  order_data    = '',
					  reg_datetime    = getdate()
				WHERE out_device_id  = @out_device_id
				  AND spool_point    = @_WritePoint;

			END

		end

		--//*************************************************************************
		--// Update new information of spool table
		--//*************************************************************************
		set @_STEPNUM	= 240;

		--// Increase Read Pooint
		set @_TEMPPOINT = @_NewRPOINT + 1;
		if (@_TEMPPOINT > @sIZE)
		begin
		  set @_TEMPPOINT = @_TEMPPOINT - @sIZE;
		end
		set @_NewRPOINT = @_TEMPPOINT;

		--// Increase Read Pooint
		set @_TEMPPOINT = @_NewEPOINT + 1;
		if (@_TEMPPOINT > @sIZE)
		begin
		  set @_TEMPPOINT = @_TEMPPOINT - @sIZE;
		end
		set @_NewEPOINT = @_TEMPPOINT;

		UPDATE SS_SPOOL_WRITE SET
           write_point = 1,
           data_count    = @sIZE
		WHERE out_device_id  = @out_device_id;

		--//*************************************************************************
		--// Delete Old Row
		--//*************************************************************************
		set @_STEPNUM	= 250;
		if (@data_count > @sIZE)
		begin
			DELETE FROM SS_SPOOL_DATA
			WHERE out_device_id  = @out_device_id
			 AND spool_point BETWEEN @sIZE+1 AND @data_count;
		end

		--//*************************************************************************
		--// Delete table for temporary
		--//*************************************************************************
		set @_STEPNUM	= 260;

		DELETE FROM SS_SPOOL_TEMP
		WHERE out_device_id   = @out_device_id;

		set @_STEPNUM	= 999;
		set @_RETURN = 0;

		COMMIT TRAN;

	END TRY

	BEGIN CATCH

		ROLLBACK TRAN;

		if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_DELETE, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_NUMBER() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);

	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.10.21
-- Description:	Change full count Spool
-- Usage:
--DECLARE @out_device_id	char(7),
--		@tHREHOLD	int,
--		@_RETURN	int ,
--		@_ERRMESG	nvarchar(256)
--
--SET @out_device_id = 'ABCD';
--SET @tHREHOLD = 95;
--
--exec USP_SPOOL_SET_THRESHOLD @out_device_id, @tHREHOLD, @_RETURN output, @_ERRMESG output
--
--select @_RETURN, @_ERRMESG
-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_SET_THRESHOLD]
(
	@out_device_id	char(7),
	@tHREHOLD	int,
	@_RETURN	int output,
	@_ERRMESG	nvarchar(256)	output
)
AS
BEGIN
-- SET NOCOUNT ON added to prevent extra result sets from
SET NOCOUNT OFF;

DECLARE @_STEPNUM	int,
		@_DETAIL	nvarchar(300),
		@_FULL_CNT        int

set @_STEPNUM = 0;
set @_FULL_CNT = 0;

set @_RETURN  = 0;
set @_ERRMESG  = '';

BEGIN TRY
	BEGIN TRAN;

		set @_STEPNUM = 10;

		set @_FULL_CNT = isnull((SELECT full_count FROM SS_SPOOL_WRITE WHERE out_device_id = @out_device_id), -1);

		set @_STEPNUM = 20;

		if (@_FULL_CNT < 0)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
					11, -- Severity,  
					2 -- Message id. -- State,  
					);
		end

		set @_STEPNUM = 30;

		if (@_FULL_CNT = @tHREHOLD)
		begin
			--e_NO_CHANGE
			RAISERROR (N'INPUTED SIZE IS NOT CHANGED',  
					11, -- Severity,  
					0 -- Message id. -- State,  
					);
		end

		set @_STEPNUM = 40;

		UPDATE SS_SPOOL_WRITE
		   SET full_count = @tHREHOLD,
			   reg_datetime      = getdate()
		 WHERE out_device_id = @out_device_id;

		 set @_STEPNUM = 50;

		 if (@@ROWCOUNT != 1)
		 begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
					11, -- Severity,  
					2 -- Message id. -- State,  
					);
		 end

		COMMIT TRAN;

	END TRY

	BEGIN CATCH

		ROLLBACK TRAN;

      if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_SET_THRESHOLD, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_STATE() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);

	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.10.21
-- Description:	Spool Change Point
-- Usage:
-- DECLARE @out_device_id	char(7),
--	@wPOINT	int,
--	@rPOINT	int,
--	@ePOINT	int,
--	@_RETURN	int,
--	@_ERRMESG	nvarchar(256)

--SET @out_device_id = 'ABCD'
--SET @wPOINT = 90
--SET @rPOINT = 90
--SET @ePOINT = 90

--exec USP_SPOOL_CHG_POINT @out_device_id, @wPOINT, @rPOINT, @ePOINT, @_RETURN output, @_ERRMESG output
--SELECT @_RETURN, @_ERRMESG
-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_CHG_POINT]
(
	@out_device_id	char(7),
	@wPOINT	int,
	@rPOINT	int,
	@ePOINT	int,
	@_RETURN	int output,
	@_ERRMESG	nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT OFF;

	DECLARE @_STEPNUM	int,
			@_DETAIL	nvarchar(300),

			@vnu_SIZE            int,
			@vnu_WPOINT          int,
			@vnu_RPOINT          int,
			@vnu_EPOINT          int,

			@tnu_WPOINT          int,
			@tnu_RPOINT          int,
			@tnu_EPOINT          int;

	set @_STEPNUM	= 0;

	set @vnu_SIZE	= 0;
	set @vnu_WPOINT	= 0;
	set @vnu_RPOINT	= 0;
	set @vnu_EPOINT	= 0;

	set @tnu_WPOINT	= 0;
	set @tnu_RPOINT	= 0;
	set @tnu_EPOINT	= 0;

	set @_RETURN  = 0 ;
	set @_ERRMESG  = '';

	BEGIN TRY

		BEGIN TRAN;

		set @_STEPNUM	= 10;

		set @vnu_SIZE = ISNULL((SELECT data_count FROM SS_SPOOL_WRITE WHERE out_device_id = @out_device_id), -1);
		set @vnu_WPOINT = ISNULL((SELECT write_point FROM SS_SPOOL_WRITE WHERE out_device_id = @out_device_id), -1);
		set @vnu_RPOINT = ISNULL((SELECT read_point FROM SS_SPOOL_READ WHERE out_device_id = @out_device_id), -1);
		set @vnu_EPOINT = ISNULL((SELECT exe_point FROM SS_SPOOL_READ WHERE out_device_id = @out_device_id), -1);

		set @_STEPNUM	= 20;

		if (@vnu_SIZE < 0 or @vnu_WPOINT < 0 or @vnu_RPOINT < 0 or @vnu_EPOINT < 0)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				11, -- Severity,  
				2 -- Message id. -- State,  
				);
		end

		set @_STEPNUM	= 30;

		if (@wPOINT < 1)
		begin
		  set @tnu_WPOINT = @vnu_WPOINT;
		end
		else
		begin
		  set @tnu_WPOINT = @wPOINT;
		end

		if (@rPOINT < 1)
		begin
		  set @tnu_RPOINT = @vnu_RPOINT;
		end
		else
		begin
		  set @tnu_RPOINT = @rPOINT;
		end

		if (@ePOINT < 1)
		begin
		  set @tnu_EPOINT = @vnu_EPOINT;
		end
		else
		begin
		  set @tnu_EPOINT = @ePOINT;
		end

		set @_STEPNUM	= 40;

		if (@tnu_WPOINT >= @tnu_EPOINT)
		begin
			set @_STEPNUM	= 50;

			if (@tnu_RPOINT < @tnu_EPOINT)
			begin
				--e_ARGS_BAD
				RAISERROR (N'ARGUMENT IS BAD(EXE POINT IS FASTER THEN READ POINT)',  
				11, -- Severity,  
				1 -- Message id. -- State,  
				);
			end

			set @_STEPNUM	= 60;

			if (@tnu_RPOINT > @tnu_WPOINT)
			begin
				--e_ARGS_BAD
				RAISERROR (N'ARGUMENT IS BAD(READ POINT IS FASTER THEN WRITE POINT)',  
				11, -- Severity,  
				1 -- Message id. -- State,  
				);
			end
		end
		else if (@tnu_RPOINT >= @tnu_WPOINT)
		begin
		  set @_STEPNUM	= 70;
		  if (@tnu_EPOINT < @tnu_WPOINT)
		  begin
			--e_ARGS_BAD
			RAISERROR (N'ARGUMENT IS BAD(EXE POINT IS FASTER THEN READ POINT)',  
				11, -- Severity,  
				1 -- Message id. -- State,  
				);
		  end

		  set @_STEPNUM	= 80;
		  if (@tnu_EPOINT > @tnu_RPOINT)
		  begin
			--e_ARGS_BAD
			RAISERROR (N'ARGUMENT IS BAD(EXE POINT IS FASTER THEN READ POINT)',  
				11, -- Severity,  
				1 -- Message id. -- State,  
				);

		  end
		end
		else if (@tnu_EPOINT >= @tnu_RPOINT)
		begin
		  set @_STEPNUM	= 90;
		  if (@tnu_WPOINT < @tnu_RPOINT)
		  begin
			--e_ARGS_BAD
			RAISERROR (N'ARGUMENT IS BAD(READ POINT IS FASTER THEN WRITE POINT)',  
				11, -- Severity,  
				1 -- Message id. -- State,  
				);
		  end
		  set @_STEPNUM	= 100;
		  if (@tnu_WPOINT > @tnu_EPOINT)
		  begin
			--e_ARGS_BAD
			RAISERROR (N'ARGUMENT IS BAD(EXE POINT IS FASTER THEN READ POINT)',  
				11, -- Severity,  
				1 -- Message id. -- State,  
				);
		  end
		end

		if (@tnu_WPOINT >= @tnu_RPOINT)
		begin
		  set @_STEPNUM	= 110;
		  if (@tnu_RPOINT > @tnu_WPOINT)
		  begin
			--e_ARGS_BAD
			RAISERROR (N'ARGUMENT IS BAD(READ POINT IS FASTER THEN WRITE POINT)',  
				11, -- Severity,  
				1 -- Message id. -- State,  
				);

		  end
		end
		else
		begin
		  set @_STEPNUM	= 120;
		  if (@tnu_RPOINT < @tnu_WPOINT)
		  begin
			--e_ARGS_BAD
			RAISERROR (N'ARGUMENT IS BAD(READ POINT IS FASTER THEN WRITE POINT)',  
				11, -- Severity,  
				1 -- Message id. -- State,  
				);

		  end
		end

		set @_STEPNUM	= 130;

		if (@ePOINT > 0)
		begin
		  UPDATE SS_SPOOL_READ
			 SET exe_point = @tnu_EPOINT,
				 reg_datetime       = getdate()
		   WHERE out_device_id = @out_device_id;

		   if (@@ROWCOUNT != 1)
			begin
			set @_DETAIL = N'SPOOL POINT IS NOTHING(POINT:'+ Cast(@tnu_EPOINT as varchar(100)) + ')';

			--e_NO_POINT
			RAISERROR (@_DETAIL,  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
			end
		end
		

		set @_STEPNUM	= 140;

		if (@wPOINT > 0)
		begin
		  UPDATE SS_SPOOL_WRITE
			 SET write_point = @tnu_WPOINT,
				 reg_datetime       = getdate()
		   WHERE out_device_id = @out_device_id;

		   if (@@ROWCOUNT != 1)
			begin
			set @_DETAIL = N'SPOOL POINT IS NOTHING(POINT:'+ Cast(@tnu_WPOINT as varchar(100)) + ')';

			--e_NO_POINT
			RAISERROR (@_DETAIL,  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
			end
		end
		
		set @_STEPNUM	= 150;

		COMMIT TRAN;

	END TRY

	BEGIN CATCH

		ROLLBACK TRAN;

		if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_CHG_POINT, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_STATE() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);

	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.10.21
-- Description:	Spool Update EXE Point
-- Usage
-- DECLARE @out_device_id	char(7),
--		@kEY	nvarchar(50),
--
--		@_ePOINT	int	,
--		@_RETURN	int ,
--		@_ERRMESG	nvarchar(256)
--
--SET @out_device_id = 'ABCD';
--SET @kEY = 'A';
--
--exec USP_SPOOL_UPT_EPOINT @out_device_id, @kEY, @_ePOINT output, @_RETURN output, @_ERRMESG output
--
--select @_ePOINT, @_RETURN, @_ERRMESG
-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_UPT_EPOINT]
(
	@out_device_id	char(7),
	@kEY	nvarchar(50),
	@_ePOINT	int	output,
	@_RETURN	int output,
	@_ERRMESG	nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT OFF;

	DECLARE @_STEPNUM	int,
			@_DETAIL	nvarchar(300),

			@vnu_SIZE            int,
			@vnu_WPOINT          int,
			@vnu_RPOINT          int,
			@vnu_EPOINT          int,

			@vnu_POINT1          int,
			@vnu_POINT2          int,
			@vnu_SPOINT          int,
			@vbo_FIND          bit,

			@vnu_TempRPOINT          int,
			@vnu_TempPOINT          int;

	set @_STEPNUM	= 0;

	set @vnu_SIZE	= 0;
	set @vnu_WPOINT	= 0;
	set @vnu_RPOINT	= 0;
	set @vnu_EPOINT	= 0;

	set @vnu_POINT1	= 0;
	set @vnu_POINT2	= 0;
	set @vnu_SPOINT	= 0;
	set @vbo_FIND	= 0;

	set @vnu_TempRPOINT	= 0;
	set @vnu_TempPOINT	= 0;

	set @_ePOINT = 0;
	set @_RETURN  = 0;
	set @_ERRMESG  = '';

	BEGIN TRY

		set @_STEPNUM	= 10;

		set @vnu_SIZE = ISNULL((SELECT data_count FROM SS_SPOOL_WRITE WHERE out_device_id = @out_device_id), -1);
		set @vnu_WPOINT = ISNULL((SELECT write_point FROM SS_SPOOL_WRITE WHERE out_device_id = @out_device_id), -1);
		set @vnu_RPOINT = ISNULL((SELECT read_point FROM SS_SPOOL_READ WHERE out_device_id = @out_device_id), -1);
		set @vnu_EPOINT = ISNULL((SELECT exe_point FROM SS_SPOOL_READ WHERE out_device_id = @out_device_id), -1);

		set @_STEPNUM	= 20;

		if (@vnu_SIZE < 0 or @vnu_WPOINT < 0 or @vnu_RPOINT < 0 or @vnu_EPOINT < 0)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				   11, -- Severity,  
				   2 -- Message id. -- State,  
				   );
		end

		else
		begin

			set @_STEPNUM	= 30;

			if (@vnu_EPOINT = @vnu_RPOINT)
			begin
				set @_STEPNUM	= 40;

				--e_NO_DATA
				RAISERROR (N'MATCHING DATA IS NOTHING',  
				   11, -- Severity,  
				   100 -- Message id. -- State,  
				   );

			end
			else if (@vbo_FIND = 0 and @vnu_EPOINT < @vnu_RPOINT)
			begin
				set @_STEPNUM	= 50;

				SET @vnu_POINT1 = @vnu_EPOINT;
				SET @vnu_POINT2 = @vnu_RPOINT-1;

				DECLARE @cur_spool_point int,
						@cur_spool_data_id	char(2),
						@cur_spool_key	char(50)

				DECLARE my_cur_1 CURSOR FOR

				SELECT spool_point, spool_data_id, spool_key FROM SS_SPOOL_DATA
				WHERE out_device_id = @out_device_id
						AND spool_point BETWEEN @vnu_POINT1 AND @vnu_POINT2
				ORDER BY spool_point ASC;
			
				OPEN my_cur_1

				FETCH NEXT FROM my_cur_1 INTO @cur_spool_point, @cur_spool_data_id, @cur_spool_key

				WHILE (@@FETCH_STATUS=0)
				BEGIN
					IF ((substring(@cur_spool_data_id, 1, 1) = 'S') and RTRIM(@kEY) = RTRIM(@cur_spool_key))
					begin
						SET @vbo_FIND   = 1;
						SET @vnu_EPOINT = @cur_spool_point;
						break;
					end
			
					FETCH NEXT FROM my_cur_1 INTO @cur_spool_point, @cur_spool_data_id, @cur_spool_key
				END

				CLOSE my_cur_1
				DEALLOCATE my_cur_1

				set @_STEPNUM	= 60;

				IF (@vbo_FIND = 0)
				BEGIN
					set @_STEPNUM	= 70;

					SET @vnu_POINT1 = 1;
					SET @vnu_POINT2 = @vnu_RPOINT-1;

					DECLARE my_cur_2 CURSOR FOR

					SELECT spool_point, spool_data_id, spool_key FROM SS_SPOOL_DATA
					WHERE out_device_id = @out_device_id
							AND spool_point BETWEEN @vnu_POINT1 AND @vnu_POINT2
					ORDER BY spool_point ASC;
			
					OPEN my_cur_2

					FETCH NEXT FROM my_cur_2 INTO @cur_spool_point, @cur_spool_data_id, @cur_spool_key

					WHILE (@@FETCH_STATUS=0)
					BEGIN
						IF ((substring(@cur_spool_data_id, 1, 1) = 'S') and RTRIM(@kEY) = RTRIM(@cur_spool_key))
						begin
							SET @vbo_FIND   = 1;
							SET @vnu_EPOINT = @cur_spool_point;
							break;
						end
			
						FETCH NEXT FROM my_cur_2 INTO @cur_spool_point, @cur_spool_data_id, @cur_spool_key
					END

					CLOSE my_cur_2
					DEALLOCATE my_cur_2
				END

				set @_STEPNUM	= 80;

				--SELECT @_STEPNUM

				if (@vbo_FIND = 0)
				begin
					set @_STEPNUM	= 90;
					set @_DETAIL = N'SPOOL POINT IS NOTHING(POINT:' + @kEY + ')';
			
					--e_NO_POINT
					RAISERROR (@_DETAIL,  
					   11, -- Severity,  
					   3 -- Message id. -- State,  
					   );

				end

				set @_STEPNUM	= 100;

				SET @vnu_TempPOINT = @vnu_EPOINT + 1

				if (@vnu_TempPOINT > @vnu_SIZE)
				begin
					SET @vnu_TempPOINT = @vnu_TempPOINT - @vnu_SIZE;
				end

				set @_STEPNUM	= 110;

				UPDATE SS_SPOOL_READ 
				SET exe_point = @vnu_TempPOINT,
				   reg_datetime       = getdate()
				WHERE out_device_id = @out_device_id

				if (@@ROWCOUNT != 1)
				begin
					set @_DETAIL = N'SPOOL POINT IS NOTHING(POINT:'+ Cast(@vnu_EPOINT as varchar(100)) + ')';

					--e_NO_POINT
					RAISERROR (@_DETAIL,  
					   11, -- Severity,  
					   3 -- Message id. -- State,  
					   );

				end
				else
				begin
					set @_STEPNUM	= 150;
					set @_EPOINT = @vnu_EPOINT;
					set @_RETURN = 0;
				end

			end

			
		end

	END TRY

	BEGIN CATCH

		if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_CHG_POINT, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_STATE() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);

	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.12.05
-- Description:	Get Spool Wait Value
-- Usage:
--DECLARE @out_device_id	char(7),
--		@_WPOINT	int,
--		@_RETURN	int ,
--		@_ERRMESG	nvarchar(256)

--SET @out_device_id = 'ABCD';

--exec USP_SPOOL_WAIT @out_device_id, @_WPOINT output, @_RETURN output, @_ERRMESG output

--select @_WPOINT, @_RETURN, @_ERRMESG
-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_WAIT]
(
	@out_device_id	char(7),
	@_WPOINT	int output,
	@_RETURN	int output,
	@_ERRMESG	nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	SET NOCOUNT ON;

	DECLARE @_STEPNUM	int,
			@vnu_WPOINT          int,
			@vnu_RPOINT          int,
			@vnu_EPOINT          int,
			@vnu_SIZE            int

	set @_STEPNUM	= 0;
	set @vnu_WPOINT          = 0;
	set @vnu_RPOINT          = 0;
	set @vnu_EPOINT          = 0;
	set @vnu_SIZE            = 0;

	set @_WPOINT = 0;
	set @_RETURN  = 0;
	set @_ERRMESG  = '';

	BEGIN TRY

		set @_STEPNUM = 10;

		SELECT @vnu_WPOINT = A.write_point, 
				@vnu_RPOINT = B.read_point, 
				@vnu_EPOINT = B.exe_point, 
				@vnu_SIZE = A.data_count
		FROM SS_SPOOL_WRITE A INNER JOIN SS_SPOOL_READ B
				ON A.out_device_id = B.out_device_id
		WHERE A.out_device_id = @out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				11, -- Severity,  
				2 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 20;

		IF (@vnu_WPOINT != @vnu_RPOINT)
		begin
			set @_STEPNUM = 30;

			SET @_WPOINT = @vnu_WPOINT;
			SET @_RETURN = 0;
			RETURN;
		end

		set @_STEPNUM = 40;

		IF (@vnu_WPOINT = @vnu_RPOINT)
		begin
			set @_RETURN  = 5;
			RETURN;
		end

		SET @_WPOINT = @vnu_WPOINT;

		IF (@_WPOINT = 0)
		begin
			--e_NO_DATA
			RAISERROR (N'MATCHING DATA IS NOTHING',  
				11, -- Severity,  
				100 -- Message id. -- State,  
				);
		end

		set @_RETURN  = 0;

	END TRY

	BEGIN CATCH

		if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_WAIT, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_STATE() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);


	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.12.05
-- Description:	Set Spool Clear
-- Usage:
--DECLARE @out_device_id	char(7),
--		@_RETURN	int ,
--		@_ERRMESG	nvarchar(256)

--SET @out_device_id = 'ABCD';

--exec USP_SPOOL_CLEAR @out_device_id, @_RETURN output, @_ERRMESG output

--select @_RETURN, @_ERRMESG
-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_CLEAR]
(
	@out_device_id	char(7),
	@_RETURN	int output,
	@_ERRMESG	nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	SET NOCOUNT ON;

	DECLARE @_STEPNUM	int,
			@vnu_WPOINT          int,
			@vnu_RPOINT          int,
			@vnu_EPOINT          int,
			@vnu_SIZE            int,

			@vch_WPOINT          CHAR(10),
			@vnu_LENDATA         int,
			@vnu_TempPOINT       int,
			@vnu_TempEPOINT      int


	set @_STEPNUM	= 0;
	set @vnu_WPOINT          = 0;
	set @vnu_RPOINT          = 0;
	set @vnu_EPOINT          = 0;
	set @vnu_SIZE            = 0;

	set @vch_WPOINT			= '0';
	set @vnu_LENDATA         = 0;
	set @vnu_TempPOINT       = 0;
	set @vnu_TempEPOINT      = 0;

	set @_RETURN  = 0;
	set @_ERRMESG  = '';

	BEGIN TRY

		BEGIN TRAN;

		set @_STEPNUM = 10;

		UPDATE SS_SPOOL_WRITE
		SET write_point = 1,
           reg_datetime      = getdate()
     	WHERE out_device_id = @out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				11, -- Severity,  
				2 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 20;

		UPDATE SS_SPOOL_READ
		SET read_point  = 1,
           exe_point   = 1,
           reg_datetime      = getdate()
     	WHERE out_device_id = @out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				11, -- Severity,  
				2 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 30;

		UPDATE SS_SPOOL_DATA
		SET spool_data_id = '',
			plant_date    = '',
           station_code     = '',
           station_seq    = '',
			spool_key     = '',
           data_size  = 0,
           order_data   = '',
           reg_datetime     = getdate()
     	WHERE out_device_id = @out_device_id

		if (@@ROWCOUNT < 1)
		begin
			--e_NO_POINT
			RAISERROR (N'SPOOL POINT IS NOTHING',  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
		end

		set @_RETURN  = 0;

		COMMIT TRAN;

	END TRY

	BEGIN CATCH

		ROLLBACK TRAN;

		if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_CLEAR, DEVICE:' + @out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_STATE() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);


	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.12.05
-- Description:	Spool Write
-- Usage:
--DECLARE @pch_out_device_id	char(7),
--		@pch_spool_data_id     char(2),
--		@pch_plant_date      char(8),
--		@pch_station_code     char(5),
--		@pch_station_seq   char(4),
--		@pch_spool_key			char(50),
--		@pte_order_data	nvarchar(max),

--		@_WPOINT   int,
--		@_RETURN	int ,
--		@_ERRMESG	nvarchar(256)

--SET @pch_out_device_id = 'ABCD';
--SET @pch_spool_data_id = '@pch_spool_data_id';
--SET @pch_plant_date = '@pch_plant_date';
--SET @pch_station_code = '@pch_station_code';
--SET @pch_station_seq = '@pch_station_seq';
--SET @pch_spool_key = '@pch_spool_key';
--SET @pte_order_data = '@pte_order_data';


--exec USP_SPOOL_WRITE @pch_out_device_id, @pch_spool_data_id, @pch_plant_date, @pch_station_code,
--					@pch_station_seq, @pch_spool_key, @pte_order_data,
--					@_WPOINT output, @_RETURN output, @_ERRMESG output

--select @_WPOINT, @_RETURN, @_ERRMESG

-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_WRITE]
(
	@pch_out_device_id	char(7),
	@pch_spool_data_id     char(2),
    @pch_plant_date      char(8),
    @pch_station_code     char(5),
    @pch_station_seq   char(4),
    @pch_spool_key			char(50),
    @pte_order_data	nvarchar(max),

    @_WPOINT   int	output,
    @_RETURN   int	output,
    @_ERRMESG  nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	SET NOCOUNT ON;

	DECLARE @_STEPNUM	int,
			@vvc_DETAIL	nvarchar(1000),
			@vnu_WPOINT          int,
			@vnu_RPOINT          int,
			@vnu_EPOINT          int,
			@vnu_SIZE            int,
			@vnu_FCNT            int,

			@vnu_LENDATA         int,

			@vnu_TempWPOINT      int,
			@vnu_TempRPOINT      int,
			@vnu_TempEPOINT      int,
			@vnu_FULL_FLG        int


	set @_STEPNUM	= 0;
	set @vvc_DETAIL = '';

	set @vnu_WPOINT          = 0;
	set @vnu_RPOINT          = 0;
	set @vnu_EPOINT          = 0;
	set @vnu_SIZE            = 0;
	set @vnu_FCNT            = 0;

	set @vnu_LENDATA         = 0;

	set @vnu_TempWPOINT      = 0;
	set @vnu_TempRPOINT      = 0;
	set @vnu_TempEPOINT      = 0;
	set @vnu_FULL_FLG        = 0;

	set @_RETURN  = 0;
	set @_ERRMESG  = '';

	BEGIN TRY

		BEGIN TRAN;

		set @_STEPNUM = 10;

		SELECT @vnu_WPOINT = write_point, @vnu_SIZE = data_count, @vnu_FCNT = full_count
		FROM SS_SPOOL_WRITE
		WHERE out_device_id = @pch_out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				11, -- Severity,  
				2 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 20;

		SELECT @vnu_RPOINT = read_point , @vnu_EPOINT = exe_point
		  FROM SS_SPOOL_READ
		WHERE out_device_id = @pch_out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_POINT
			RAISERROR (N'SPOOL POINT IS NOTHING',  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 30;

		SET @vnu_TempRPOINT = @vnu_RPOINT;
        SET @vnu_TempEPOINT = @vnu_EPOINT;

		SET @vnu_TempEPOINT = @vnu_TempEPOINT - 1;

		IF (@vnu_TempEPOINT < 1)
		begin
            SET @vnu_TempEPOINT = @vnu_TempEPOINT + @vnu_SIZE;
		end

		set @_STEPNUM = 40;

		IF (@vnu_TempEPOINT = @vnu_WPOINT)
		begin
            --RAISE e_SPOOL_FULL;
            SET @vnu_FULL_FLG = 1;
            SET @vnu_TempEPOINT = @vnu_EPOINT + 1;

            IF (@vnu_TempEPOINT > @vnu_SIZE)
			begin
                SET @vnu_TempEPOINT = @vnu_TempEPOINT - @vnu_SIZE;
			end
        end
        ELSE
		begin
            set @vnu_TempEPOINT = @vnu_EPOINT;
		end

		set @_STEPNUM = 50;

		SET @vnu_TempRPOINT = @vnu_TempRPOINT - 1;

		IF (@vnu_TempRPOINT < 1)
		begin
            SET @vnu_TempRPOINT = @vnu_TempRPOINT + @vnu_SIZE;
        end


		IF (@vnu_TempRPOINT = @vnu_WPOINT)
		begin
            --RAISE e_SPOOL_FULL;
            SET @vnu_FULL_FLG = 1;
            SET @vnu_TempRPOINT = @vnu_RPOINT + 1;
            
			IF (@vnu_TempRPOINT > @vnu_SIZE)
			begin
                SET @vnu_TempRPOINT = @vnu_TempRPOINT - @vnu_SIZE;
            end
		end
        ELSE
		begin
            SET @vnu_TempRPOINT = @vnu_RPOINT;
		end


		SET @_STEPNUM   = 60;
        SET @vnu_TempWPOINT = @vnu_WPOINT;
        SET @_WPOINT    = @vnu_WPOINT;
        SET @vnu_LENDATA   = LEN(cast(@pte_order_data as nvarchar(max)));

		SET @_STEPNUM   = 70;

		UPDATE SS_SPOOL_DATA
		SET spool_data_id = @pch_spool_data_id,
			plant_date    = @pch_plant_date,
           station_code     = @pch_station_code,
           station_seq    = @pch_station_seq,
			spool_key     = @pch_spool_key,
           data_size  = @vnu_LENDATA,
           order_data   = @pte_order_data,
           reg_datetime     = getdate()
     	WHERE out_device_id = @pch_out_device_id
			and spool_point = @vnu_TempWPOINT

		if (@@ROWCOUNT != 1)
		begin
			SET @vvc_DETAIL = N'SPOOL POINT IS NOTHING(POINT:' + Cast(@vnu_TempWPOINT as varchar(100)) + ')';

			--e_NO_POINT
			RAISERROR (@vvc_DETAIL,  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
		end

		SET @_STEPNUM   = 80;

		SET @vnu_TempWPOINT = @vnu_WPOINT + 1;

		IF (@vnu_TempWPOINT > @vnu_SIZE)
		begin
            SET @vnu_TempWPOINT = @vnu_TempWPOINT - @vnu_SIZE;
        end

		SET @_STEPNUM   = 90;

		UPDATE SS_SPOOL_WRITE
           SET write_point = @vnu_TempWPOINT,
               reg_datetime     = getdate()
         WHERE out_device_id = @pch_out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				11, -- Severity,  
				2 -- Message id. -- State,  
				);
		end


		IF (@vnu_FCNT < 1 OR @vnu_FULL_FLG != 0)
		begin
            SET @_STEPNUM   = 100;

			SELECT @vnu_RPOINT = read_point , @vnu_EPOINT = exe_point
			  FROM SS_SPOOL_READ
			WHERE out_device_id = @pch_out_device_id

			if (@@ROWCOUNT != 1)
			begin
				SET @vvc_DETAIL = N'SPOOL POINT IS NOTHING)';

				--e_NO_POINT
				RAISERROR (@vvc_DETAIL,  
					11, -- Severity,  
					3 -- Message id. -- State,  
					);
			end

			SET @_STEPNUM   = 110;

            UPDATE SS_SPOOL_READ
               SET read_point   = @vnu_TempRPOINT,
                   exe_point    = @vnu_TempEPOINT,
                   reg_datetime       = getdate()
             WHERE out_device_id = @pch_out_device_id
		end

		SET @_STEPNUM   = 120;

		SET @_RETURN  = 0;

		COMMIT TRAN;

	END TRY

	BEGIN CATCH

		ROLLBACK TRAN;

		if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_WRITE, DEVICE:' + @pch_out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_STATE() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);


	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.12.05
-- Description:	Spool Read
-- Usage:
--DECLARE @pch_out_device_id	char(7),
--		@pnu_UPTEPOINT		int,
--		@pnu_spool_point     int,
--		@pch_spool_data_id     char(2),
--		@pch_plant_date      char(8),
--		@pch_station_code     char(5),
--		@pch_station_seq   char(4),
--		@pch_spool_key			char(50),
--		@pnu_data_size			int,
--		@pte_order_data	varchar(max),

--		@_RETURN   int,
--		@_ERRMESG  nvarchar(256)

--SET @pch_out_device_id = 'ABCD';
--SET @pnu_UPTEPOINT = 1;

--exec USP_SPOOL_READ @pch_out_device_id, @pnu_UPTEPOINT,
--					@pnu_spool_point output, @pch_spool_data_id output, 
--					@pch_plant_date output, @pch_station_code output,
--					@pch_station_seq output, @pch_spool_key output, 
--					@pnu_data_size output, @pte_order_data output,
--					@_RETURN output, @_ERRMESG output

--select @pnu_spool_point, @pch_spool_data_id,
--		@pch_plant_date, @pch_station_code,
--		@pch_station_seq, @pch_spool_key,
--		@pnu_data_size, @pte_order_data,
--		@_RETURN, @_ERRMESG

-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_READ]
(
	@pch_out_device_id	char(7),
	@pnu_UPTEPOINT		int,
	@pnu_spool_point     int output,
	@pch_spool_data_id     char(2) output,
    @pch_plant_date      char(8) output,
    @pch_station_code     char(5) output,
    @pch_station_seq   char(4) output,
    @pch_spool_key			char(50) output,
	@pnu_data_size			int output,
    @pte_order_data	varchar(max) output,

    @_RETURN   int	output,
    @_ERRMESG  nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	SET NOCOUNT ON;

	DECLARE @_STEPNUM	int,
			@vvc_DETAIL	nvarchar(1000),
			
			@vnu_WPOINT          int,
			@vnu_RPOINT          int,
			@vnu_EPOINT          int,
			@vnu_SIZE            int,
			@vnu_TempRPOINT      int,
			@log_txt	varchar(max)


	set @_STEPNUM	= 0;
	set @vvc_DETAIL = '';

	set @vnu_WPOINT          = 0;
	set @vnu_RPOINT          = 0;
	set @vnu_EPOINT          = 0;
	set @vnu_SIZE            = 0;
	set @vnu_TempRPOINT      = 0;

	set @_RETURN  = 0;
	set @_ERRMESG  = '';

	BEGIN TRY

		BEGIN TRAN;

		set @_STEPNUM = 10;

		SELECT @vnu_WPOINT = write_point, @vnu_SIZE = data_count
		FROM SS_SPOOL_WRITE
		WHERE out_device_id = @pch_out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				11, -- Severity,  
				2 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 20;

		SELECT @vnu_RPOINT = read_point , @vnu_EPOINT = exe_point
		  FROM SS_SPOOL_READ
		WHERE out_device_id = @pch_out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_POINT
			RAISERROR (N'SPOOL POINT IS NOTHING',  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 30;

		IF (@vnu_RPOINT = @vnu_WPOINT)
		begin
			--e_NO_DATA
			RAISERROR (N'READING DATA IS NOTHING',  
				11, -- Severity,  
				100 -- Message id. -- State,  
				);
        end


		set @_STEPNUM = 40;

		SELECT @pnu_spool_point = spool_point , @pch_spool_data_id = spool_data_id, @pch_plant_date = plant_date, 
				@pch_station_code = station_code, @pch_station_seq = station_seq, @pch_spool_key = spool_key, 
				@pnu_data_size = data_size, @pte_order_data = order_data
		  FROM SS_SPOOL_DATA
		WHERE out_device_id = @pch_out_device_id
			and spool_point = @vnu_RPOINT

		if (@@ROWCOUNT != 1)
		begin
			SET @vvc_DETAIL = N'SPOOL POINT IS NOTHING(POINT: ' + Cast(@vnu_RPOINT as varchar(100)) + ')'
			--e_NO_POINT
			RAISERROR (@vvc_DETAIL,  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 50;

		set @vnu_TempRPOINT = @vnu_RPOINT + 1;

		IF (@vnu_TempRPOINT > @vnu_SIZE)
		begin
			set @vnu_TempRPOINT = @vnu_TempRPOINT - @vnu_SIZE;
        end

		set @pnu_spool_point = @vnu_RPOINT;
        set @vnu_RPOINT = @vnu_TempRPOINT;

		IF (@pnu_UPTEPOINT = 1)
		BEGIN
			UPDATE SS_SPOOL_READ
			SET read_point = @vnu_RPOINT,
				exe_point  = @vnu_RPOINT,
				reg_datetime       = getdate()
			WHERE out_device_id = @pch_out_device_id
		END
		ELSE
		BEGIN
			UPDATE SS_SPOOL_READ
			SET read_point = @vnu_RPOINT,
				reg_datetime       = getdate()
			WHERE out_device_id = @pch_out_device_id
		END

		set @_STEPNUM = 60;

		if (@@ROWCOUNT != 1)
		begin
			SET @vvc_DETAIL = N'SPOOL POINT IS NOTHING(POINT: ' + Cast(@vnu_RPOINT as varchar(100)) + ')'
			--e_NO_POINT
			RAISERROR (@vvc_DETAIL,  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 70;

		IF (@pch_spool_data_id = 'IT')
		begin
            set @pch_spool_data_id = 'IN';
		end

        IF (@pch_spool_data_id = 'ST')
		begin
            set @pch_spool_data_id = 'SN';
		end

		SET @_STEPNUM   = 80;

		SET @_RETURN  = 0;

		COMMIT TRAN;

	END TRY

	BEGIN CATCH

		ROLLBACK TRAN;

		if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_READ, DEVICE:' + @pch_out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_STATE() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);


	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.12.05
-- Description:	Spool RETRIEVE
-- Usage:
--DECLARE @pch_out_device_id	char(7),
--		@pnu_EPOINT     int,
--		@pch_spool_data_id     char(2),
--		@pch_plant_date      char(8),
--		@pch_station_code     char(5),
--		@pch_station_seq   char(4),
--		@pch_spool_key			char(50),
--		@pnu_data_size			int,
--		@pte_order_data	varchar(max),

--		@_RETURN   int,
--		@_ERRMESG  nvarchar(256)

--SET @pch_out_device_id = 'ABCD';
--SET @pnu_EPOINT = 1;

--exec USP_SPOOL_RETRIEVE @pch_out_device_id, 
--					@pnu_EPOINT output, @pch_spool_data_id output, 
--					@pch_plant_date output, @pch_station_code output,
--					@pch_station_seq output, @pch_spool_key output, 
--					@pnu_data_size output, @pte_order_data output,
--					@_RETURN output, @_ERRMESG output

--select @pnu_EPOINT, @pch_spool_data_id,
--		@pch_plant_date, @pch_station_code,
--		@pch_station_seq, @pch_spool_key,
--		@pnu_data_size, @pte_order_data,
--		@_RETURN, @_ERRMESG

-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_RETRIEVE]
(
	@pch_out_device_id	char(7),
	@pnu_EPOINT     int output,
	@pch_spool_data_id     char(2) output,
    @pch_plant_date      char(8) output,
    @pch_station_code     char(5) output,
    @pch_station_seq   char(4) output,
    @pch_spool_key			char(50) output,
	@pnu_data_size			int output,
    @pte_order_data	varchar(max) output,

    @_RETURN   int	output,
    @_ERRMESG  nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	SET NOCOUNT ON;

	DECLARE @_STEPNUM	int,
			@vvc_DETAIL	nvarchar(1000),

			@vnu_WPOINT          int,
			@vnu_RPOINT          int,
			@vnu_EPOINT          int,
			@vnu_SIZE            int,

			@vnu_SPOINT          int,
			@vnu_SP_DataID		char(2),
			@vnu_SCOUNT          int,
			@vnu_RETR_PNT        int,
			@vnu_RCOUNT          int,
			@vnu_ROOPCNT         int,

			@vnu_TempEPOINT      int;




	set @_STEPNUM	= 0;
	set @vvc_DETAIL = '';

	set @vnu_WPOINT = 0;
	set @vnu_RPOINT = 0;
	set @vnu_EPOINT = 0;
	set @vnu_SIZE = 0;

	set @vnu_SPOINT = 0;
	set @vnu_SP_DataID = '';
	set @vnu_SCOUNT = 0;
	set @vnu_RETR_PNT = 0;
	set @vnu_RCOUNT = 0;
	set @vnu_ROOPCNT = 0;

	set @vnu_TempEPOINT = 0;

	set @pch_spool_data_id   = '';
    set @pch_plant_date     = '';
    set @pch_station_code     = '';
    set @pch_station_seq   = '';
    set @pch_spool_key		= '';
	set @pnu_data_size		= 0;
    set @pte_order_data		= '';


	set @_RETURN  = 0;
	set @_ERRMESG  = '';

	BEGIN TRY

		set @_STEPNUM = 10;

		SELECT @vnu_WPOINT = A.write_point, @vnu_SIZE = A.data_count, @vnu_RPOINT = B.read_point, @vnu_EPOINT = B.exe_point
		FROM SS_SPOOL_WRITE A inner join SS_SPOOL_READ B
		ON A.out_device_id = B.out_device_id
		WHERE A.out_device_id = @pch_out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				11, -- Severity,  
				2 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 20;

		if (@pnu_EPOINT > 0)
		begin
			SET @vnu_EPOINT = @pnu_EPOINT + 1;

			if (@vnu_EPOINT > @vnu_SIZE)
			begin
				SET @vnu_EPOINT = 1;
			end

			set @_STEPNUM = 30;

			if ((@vnu_EPOINT = @vnu_RPOINT) or (@vnu_EPOINT = @vnu_WPOINT))
			begin
				--e_NO_DATA
				RAISERROR (N'READING DATA IS NOTHING',  
					11, -- Severity,  
					100 -- Message id. -- State,  
					);
			end

		end 
		else
		begin
			SET @_STEPNUM		= 40;

			SET @vnu_SPOINT     = 0;
            SET @vnu_RCOUNT     = @pnu_EPOINT*-1;

			if (@vnu_RCOUNT > @vnu_SIZE)
			begin
				SET @vnu_RCOUNT = @vnu_SIZE;
			end

			SET @vnu_RETR_PNT = @vnu_EPOINT - 1;

			if (@vnu_RETR_PNT < 1)
			begin
				SET @vnu_RETR_PNT = @vnu_SIZE;
			end

			SET @vnu_EPOINT = 0;

			SELECT @vnu_SPOINT = spool_point, @vnu_SP_DataID = spool_data_id
			FROM SS_SPOOL_DATA
			WHERE out_device_id = @pch_out_device_id
			and spool_point between @vnu_RETR_PNT - 200 and @vnu_RETR_PNT
			ORDER BY spool_point DESC

			if (@@ROWCOUNT < 1)
			begin
				SET @_STEPNUM		= 50;

				if (@vnu_SPOINT = 1)
				begin
					SET @vnu_RETR_PNT = @vnu_SIZE;
				end
				else
				begin
					SET @vnu_RETR_PNT = @vnu_RETR_PNT - 200;
				end
			end
			else
			begin
				SET @vnu_ROOPCNT = @vnu_ROOPCNT + 1;

				SET @_STEPNUM		= 60;

				if (SUBSTRING(@vnu_SP_DataID, 1, 1) = '')
				begin
					SET @_STEPNUM		= 70;
					--EXIT;	
				end
			end

			SET @vnu_EPOINT = @vnu_SPOINT;

			if (@vnu_SPOINT != @vnu_WPOINT and (@vnu_SP_DataID = 'SN' or @vnu_SP_DataID = 'SR' or @vnu_SP_DataID = 'ST'))
			begin
				SET @vnu_SCOUNT = @vnu_SCOUNT + 1;
			end

			SET @_STEPNUM		= 80;

			if (@vnu_SPOINT = @vnu_WPOINT)
			begin
				--EXIT;
				SET @_STEPNUM		= 90;
			end

			SET @_STEPNUM		= 100;

			if (@vnu_SCOUNT >= @vnu_RCOUNT)
			begin
				--EXIT;
				SET @_STEPNUM		= 110;
			end

			SET @_STEPNUM		= 120;

			if (@vnu_ROOPCNT >= @vnu_SIZE)
			begin
				--EXIT;
				SET @_STEPNUM		= 130;
			end


		end

		SET @_STEPNUM		= 140;

		if (@vnu_EPOINT < 1)
		begin
			--e_NO_DATA
			RAISERROR (N'READING DATA IS NOTHING',  
				11, -- Severity,  
				100 -- Message id. -- State,  
				);
		end

		SET @_STEPNUM		= 150;

		SELECT @pnu_EPOINT = spool_point, @pch_spool_data_id = spool_data_id, @pch_plant_date = plant_date, 
			@pch_station_code = station_code, @pch_station_seq = station_seq, @pch_spool_key = spool_key, 
			@pnu_data_size = data_size, @pte_order_data = order_data
		FROM SS_SPOOL_DATA
		WHERE out_device_id = @pch_out_device_id
		and spool_point = @vnu_EPOINT

		if (@@ROWCOUNT < 1)
		begin
			SET @vvc_DETAIL = N'SPOOL POINT IS NOTHING(POINT: ' + @vnu_EPOINT + ')';

			--e_NO_POINT
			RAISERROR (@vvc_DETAIL,  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
		end

		if (@pch_spool_data_id = 'IT')
		begin
			SET @pch_spool_data_id = 'IN';
		end

		if (@pch_spool_data_id = 'ST')
		begin
			SET @pch_spool_data_id = 'SN';
		end

		SET @_STEPNUM		= 160;


		SET @_RETURN  = 0;


	END TRY

	BEGIN CATCH

		if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_RETRIEVE, DEVICE:' + @pch_out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_STATE() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);


	END CATCH 
END
GO


SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		James Jeong (dnakorea@gmail.com)
-- Create date: 2016.12.05
-- Description:	Spool RETRIEVE
-- Usage:
--DECLARE @pch_out_device_id	char(7),
--		@pnu_EPOINT     int,
--		@pch_spool_data_id     char(2),
--		@pch_plant_date      char(8),
--		@pch_station_code     char(5),
--		@pch_station_seq   char(4),
--		@pch_spool_key			char(50),
--		@pnu_data_size			int,
--		@pte_order_data	varchar(max),

--		@_RETURN   int,
--		@_ERRMESG  nvarchar(256)

--SET @pch_out_device_id = 'ABCD';
--SET @pnu_EPOINT = 1;

--exec USP_SPOOL_ALL_RETRIEVE @pch_out_device_id, 
--					@pnu_EPOINT output, @pch_spool_data_id output, 
--					@pch_plant_date output, @pch_station_code output,
--					@pch_station_seq output, @pch_spool_key output, 
--					@pnu_data_size output, @pte_order_data output,
--					@_RETURN output, @_ERRMESG output

--select @pnu_EPOINT, @pch_spool_data_id,
--		@pch_plant_date, @pch_station_code,
--		@pch_station_seq, @pch_spool_key,
--		@pnu_data_size, @pte_order_data,
--		@_RETURN, @_ERRMESG

-- =============================================
CREATE PROCEDURE[dbo].[USP_SPOOL_ALL_RETRIEVE]
(
	@pch_out_device_id	char(7),
	@pnu_EPOINT     int output,
	@pch_spool_data_id     char(2) output,
    @pch_plant_date      char(8) output,
    @pch_station_code     char(5) output,
    @pch_station_seq   char(4) output,
    @pch_spool_key			char(50) output,
	@pnu_data_size			int output,
    @pte_order_data	varchar(max) output,

    @_RETURN   int	output,
    @_ERRMESG  nvarchar(256)	output
)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	SET NOCOUNT ON;

	DECLARE @_STEPNUM	int,
			@vvc_DETAIL	nvarchar(1000),

			@vnu_WPOINT          int,
			@vnu_RPOINT          int,
			@vnu_EPOINT          int,
			@vnu_SIZE            int,

			@vnu_SPOINT          int,
			@vnu_SP_DataID		char(2),
			@vnu_SCOUNT          int,
			@vnu_RETR_PNT        int,
			@vnu_RCOUNT          int,
			@vnu_ROOPCNT         int,

			@vnu_TempEPOINT      int;




	set @_STEPNUM	= 0;
	set @vvc_DETAIL = '';

	set @vnu_WPOINT = 0;
	set @vnu_RPOINT = 0;
	set @vnu_EPOINT = 0;
	set @vnu_SIZE = 0;

	set @vnu_SPOINT = 0;
	set @vnu_SP_DataID = '';
	set @vnu_SCOUNT = 0;
	set @vnu_RETR_PNT = 0;
	set @vnu_RCOUNT = 0;
	set @vnu_ROOPCNT = 0;

	set @vnu_TempEPOINT = 0;

	set @pch_spool_data_id   = '';
    set @pch_plant_date     = '';
    set @pch_station_code     = '';
    set @pch_station_seq   = '';
    set @pch_spool_key		= '';
	set @pnu_data_size		= 0;
    set @pte_order_data		= '';


	set @_RETURN  = 0;
	set @_ERRMESG  = '';

	BEGIN TRY

		set @_STEPNUM = 10;

		SELECT @vnu_WPOINT = A.write_point, @vnu_SIZE = A.data_count, @vnu_RPOINT = B.read_point, @vnu_EPOINT = B.exe_point
		FROM SS_SPOOL_WRITE A inner join SS_SPOOL_READ B
		ON A.out_device_id = B.out_device_id
		WHERE A.out_device_id = @pch_out_device_id

		if (@@ROWCOUNT != 1)
		begin
			--e_NO_SPOOL
			RAISERROR (N'SPOOL IS NOTHING',  
				11, -- Severity,  
				2 -- Message id. -- State,  
				);
		end

		set @_STEPNUM = 20;

		if (@pnu_EPOINT > 0)
		begin
			SET @pnu_EPOINT = @pnu_EPOINT + 1;

			if (@pnu_EPOINT > @vnu_SIZE)
			begin
				SET @pnu_EPOINT = 1;
			end

			set @_STEPNUM = 30;

			if (@pnu_EPOINT = @vnu_WPOINT)
			begin
				--e_NO_DATA
				RAISERROR (N'READING DATA IS NOTHING',  
					11, -- Severity,  
					100 -- Message id. -- State,  
					);
			end

			set @_STEPNUM = 40;

			SET @vnu_EPOINT = @pnu_EPOINT;
		end 
		else
		begin
			SET @_STEPNUM		= 50;

            SET @vnu_RCOUNT = @pnu_EPOINT*-1;

			if (@vnu_RCOUNT > @vnu_SIZE)
			begin
				SET @vnu_RCOUNT = @vnu_SIZE;
			end

			SET @vnu_RETR_PNT = @vnu_EPOINT - 1;

			if (@vnu_RETR_PNT < 1)
			begin
				SET @vnu_RETR_PNT = @vnu_SIZE;
			end

			SELECT @vnu_SPOINT = spool_point, @vnu_SP_DataID = spool_data_id
			FROM SS_SPOOL_DATA
			WHERE out_device_id = @pch_out_device_id
			and spool_point between @vnu_RETR_PNT - 200 and @vnu_RETR_PNT
			ORDER BY spool_point DESC

			if (@@ROWCOUNT < 1)
			begin
				SET @_STEPNUM		= 60;

				if (@vnu_SPOINT = 1)
				begin
					SET @vnu_RETR_PNT = @vnu_SIZE;
				end
				else
				begin
					SET @vnu_RETR_PNT = @vnu_RETR_PNT - 200;
				end
			end
			else
			begin
				SET @vnu_ROOPCNT = @vnu_ROOPCNT + 1;

				SET @_STEPNUM		= 70;

				if (@vnu_SP_DataID = '')
				begin
					SET @_STEPNUM		= 80;
					--EXIT;	
				end

				SET @vnu_EPOINT = @vnu_SPOINT;
				SET @vnu_SCOUNT = @vnu_SCOUNT + 1;

				SET @_STEPNUM		= 90;

				if (@vnu_SPOINT = @vnu_WPOINT)
				begin
					SET @_STEPNUM		= 100;
					--EXIT;	
				end

				SET @_STEPNUM		= 110;

				if (@vnu_SCOUNT >= @vnu_RCOUNT)
				begin
					SET @_STEPNUM		= 120;
					--EXIT;	
				end

				SET @_STEPNUM		= 130;

				if (@vnu_ROOPCNT > @vnu_SIZE)
				begin
					SET @_STEPNUM		= 140;
					--EXIT;	
				end

			end
		end

		SET @_STEPNUM		= 150;

		SELECT @pnu_EPOINT = spool_point, @pch_spool_data_id = spool_data_id, @pch_plant_date = plant_date, 
			@pch_station_code = station_code, @pch_station_seq = station_seq, @pch_spool_key = spool_key, 
			@pnu_data_size = data_size, @pte_order_data = order_data
		FROM SS_SPOOL_DATA
		WHERE out_device_id = @pch_out_device_id
		and spool_point = @vnu_EPOINT

		if (@@ROWCOUNT < 1)
		begin
			SET @vvc_DETAIL = N'SPOOL POINT IS NOTHING(POINT: ' + @vnu_EPOINT + ')';

			--e_NO_POINT
			RAISERROR (@vvc_DETAIL,  
				11, -- Severity,  
				3 -- Message id. -- State,  
				);
		end

		SET @_STEPNUM		= 160;

		SET @_RETURN  = 0;


	END TRY

	BEGIN CATCH

		if (ERROR_STATE() = 1) --e_ARGS_BAD
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 2) --e_NO_SPOOL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 3) --e_NO_POINT
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 4) --e_SPOOL_FULL
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 0) --e_NO_CHANGE
		begin
			set @_RETURN = ERROR_STATE();
		end
		else if (ERROR_STATE() = 100) --e_NO_DATA
		begin
			set @_RETURN = ERROR_STATE();
		end
		else
		begin
			set @_RETURN  = -1;
			--exec CHK_DB_CONNECT(@_RETURN);
		end 

		SET @_ERRMESG = 'MODOULE:USP_SPOOL_ALL_RETRIEVE, DEVICE:' + @pch_out_device_id + ', STEP:' + Cast(@_STEPNUM as nvarchar(256)) + ', SQLERR:' + Cast(ERROR_STATE() as nvarchar(257)) + ', MESG:' + ERROR_MESSAGE();
		SET @_ERRMESG = substring(@_ERRMESG, 1, 256);


	END CATCH 
END
GO

