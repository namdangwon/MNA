
--SQLMAPID: MES.FW.SP.LOG.LOG_SELECT_HI_LOG_LIST
CREATE PROCEDURE LOG_SELECT_HI_LOG_LIST
        @PageNumber int,
        @PageSize      int,
		@Search		varchar(50),
		@Keyword	nvarchar(100)
AS
        SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED
        SET NOCOUNT OFF

		DECLARE @TotalRecord int,
				@strSQL	nvarchar(2000)
		
	  exec sp_executesql N'select @TotalRecord=count(*) from HI_LOG', 
                    N'@TotalRecord int output', @TotalRecord output;

       set @PageNumber = @PageNumber  * @PageSize 
          
       if @TotalRecord < @PageNumber         
	     Set @PageSize = @PageSize - (@PageNumber - @TotalRecord)
	
		SET @strSQL = 'Select log_date, log_time, log_seq, log_type, pgm_id, proc_id, server_name, user_id,
					ui_full_name, function_name, parameters, function_duration, msg_id,
					message, stack_trace, err_message, chk_flag
        	   FROM HI_LOG '

		SET @strSQL = @strSQL + ' Where log_seq in
         (
           Select Top ' + Cast(@PageSize as varchar(10)) + ' log_seq From 
               (
                  Select Top ' +  Cast(@PageNumber as varchar(10))  + ' log_seq From HI_LOG '

		if (@Search != '' and @Keyword != '')
		begin
			SET @strSQL = @strSQL + ' WHERE ' + @Search + ' like ''' + @Keyword + ''' + ''%''';
		end

		SET @strSQL = @strSQL + ' Order by log_seq desc ) As TempTable Order by log_seq asc ) '

		if (@Search != '' and @Keyword != '')
		begin
			SET @strSQL = @strSQL + ' and ' + @Search + ' like ''' + @Keyword + ''' + ''%''';
		end

        SET @strSQL = @strSQL + ' Order by log_seq desc'
                  
	   --SELECT @strSQL
       Exec(@strSQL);
Go

--SQLMAPID: MES.FW.SP.LOG.LOG_SELECT_HI_LOG_COUNT
CREATE PROCEDURE [dbo].[LOG_SELECT_HI_LOG_COUNT]
		@Search		varchar(50),
		@Keyword	nvarchar(100)
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT OFF;

	DECLARE @strSQL	nvarchar(2000)

	if (@Search != '' and @Keyword != '')
	begin
		SET @strSQL = 'SELECT count(*) FROM HI_LOG WHERE ' + @Search + ' like ''' + @Keyword + ''' + ''%''';

		--SELECT @strSQL

		Exec(@strSQL); 
	end
	else
	begin
		SELECT count(*) FROM HI_LOG
	end
END
GO

--SQLMAPID: MES.FW.SP.LOG.LOG_SELECT_HI_LOG_READ
CREATE PROCEDURE [dbo].[LOG_SELECT_HI_LOG_READ]
		@log_date		char(8),
		@log_time		char(6),
		@log_seq		int
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT OFF;

	SELECT *
	FROM HI_LOG 
	WHERE log_date = @log_date and log_time = @log_time and log_seq = @log_seq

END
GO