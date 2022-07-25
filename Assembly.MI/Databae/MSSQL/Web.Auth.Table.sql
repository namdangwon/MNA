SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[MP_USER_SCREEN](
	[system_code] [char](2) NOT NULL,
	[user_group_code] [char](2) NOT NULL,
	[screen_id] [char](10) NOT NULL,
	[screen_authority] [char](1) NULL,
	[reg_date] [char](8) NULL,
	[reg_time] [char](6) NULL,
	[user_id] [varchar](10) NULL,
 CONSTRAINT [PK_MP_USER_SCREEN] PRIMARY KEY CLUSTERED 
(
	[system_code] ASC,
	[user_group_code] ASC,
	[screen_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO


