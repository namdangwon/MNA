CREATE TABLE [dbo].[SS_SPOOL_WRITE](
	[out_device_id] [char](7) NOT NULL,
	[write_point] [int] NULL,
	[data_count] [int] NULL,
	[full_count] [int] NULL,
	[alarm_count] [int] NULL,
	[usage_flag] [char](1) NULL,
	[reg_datetime] [datetime] NULL,
 CONSTRAINT [PK_SS_SPOOL_WRITE] PRIMARY KEY CLUSTERED 
(
	[out_device_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO


CREATE TABLE [dbo].[SS_SPOOL_DATA](
	[out_device_id] [char](7) NOT NULL,
	[spool_point] [int] NOT NULL,
	[spool_data_id] [char](2) NULL,
	[plant_date] [char](8) NULL,
	[station_code] [char](5) NULL,
	[station_seq] [char](4) NULL,
	[spool_key] [char](50) NULL,
	[data_size] [int] NULL,
	[order_data] [text] NULL,
	[reg_datetime] [datetime] NULL,
 CONSTRAINT [PK_SS_SPOOL_DATA_1] PRIMARY KEY CLUSTERED 
(
	[out_device_id] ASC,
	[spool_point] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO


CREATE TABLE [dbo].[SS_SPOOL_READ](
	[out_device_id] [char](7) NOT NULL,
	[read_point] [int] NULL,
	[exe_point] [int] NULL,
	[reg_datetime] [datetime] NULL,
 CONSTRAINT [PK_SS_SPOOL_READ] PRIMARY KEY CLUSTERED 
(
	[out_device_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO


CREATE TABLE [dbo].[SS_SPOOL_TEMP](
	[out_device_id] [char](7) NOT NULL,
	[spool_point] [int] NOT NULL,
	[spool_data_id] [char](2) NULL,
	[plant_date] [char](8) NULL,
	[station_code] [char](5) NULL,
	[station_seq] [char](4) NULL,
	[spool_key] [char](50) NULL,
	[data_size] [int] NULL,
	[order_data] [text] NULL,
	[reg_datetime] [datetime] NULL,
 CONSTRAINT [PK_SS_SPOOL_TEMP] PRIMARY KEY CLUSTERED 
(
	[out_device_id] ASC,
	[spool_point] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO