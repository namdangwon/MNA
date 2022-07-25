SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[HI_LOG](
	[log_date] [char](8) NOT NULL,
	[log_time] [char](6) NOT NULL,
	[log_seq] [bigint] IDENTITY(1,1) NOT NULL,
	[log_type] [char](1) NULL,
	[pgm_id] [varchar](30) NULL,
	[proc_id] [varchar](30) NULL,
	[server_name] [varchar](50) NULL,
	[user_id] [varchar](100) NULL,
	[ui_full_name] [varchar](100) NULL,
	[function_name] [varchar](100) NULL,
	[parameters] [varchar](500) NULL,
	[function_duration] [int] NULL,
	[msg_id] [varchar](10) NULL,
	[message] [varchar](4000) NULL,
	[stack_trace] [varchar](4000) NULL,
	[err_message] [varchar](4000) NULL,
	[chk_flag] [char](1) NULL,
 CONSTRAINT [PK_HI_LOG_MSG] PRIMARY KEY CLUSTERED 
(
	[log_date] ASC,
	[log_time] ASC,
	[log_seq] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO



SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[SS_PROC_MONITORING](
	[pgm_id] [varchar](15) NOT NULL,
	[proc_id] [varchar](15) NOT NULL,
	[pgm_name] [varchar](50) NOT NULL,
	[proc_name] [varchar](50) NOT NULL,
	[proc_state] [char](1) NULL,
	[device_status] [varchar](300) NULL,
	[heartbit_chk_sec] [int] NULL,
	[heartbit_datetime] [char](14) NULL,
	[server_ip] [varchar](15) NULL,
	[server_name] [varchar](15) NULL,
	[reg_date] [char](8) NULL,
	[reg_time] [char](6) NULL,
	[user_id] [varchar](10) NULL,
 CONSTRAINT [PK_SS_PROC_MONITORING] PRIMARY KEY CLUSTERED 
(
	[pgm_id] ASC,
	[proc_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
