SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[SS_DICTIONARY](
	[dic_id] [varchar](100) NOT NULL,
	[dic_name_ko_kr] [nvarchar](100) NULL,
	[dic_name_en_us] [nvarchar](100) NULL,
	[dic_name_lo_ln] [nvarchar](100) NULL,
	[reg_date] [char](8) NULL,
	[reg_time] [char](6) NULL,
	[user_id] [varchar](10) NULL,
 CONSTRAINT [PK_SS_DICTIONARY] PRIMARY KEY CLUSTERED 
(
	[dic_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[SS_MSG_ID](
	[msg_id] [varchar](10) NOT NULL,
	[msg_type] [char](1) NOT NULL,
	[app_type] [char](1) NOT NULL,
	[msg_direct] [varchar](2000) NULL,
	[msg_text_ko_kr] [nvarchar](200) NULL,
	[msg_text_en_us] [nvarchar](200) NULL,
	[msg_text_lo_ln] [nvarchar](200) NULL,
	[reg_date] [char](8) NULL,
	[reg_time] [char](6) NULL,
	[user_id] [varchar](10) NULL,
 CONSTRAINT [PK_SS_MSG_ID] PRIMARY KEY CLUSTERED 
(
	[msg_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
