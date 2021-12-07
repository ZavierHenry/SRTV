CREATE TYPE render_type AS ENUM ('video', 'text', 'error', 'dark image', 'light image', 'dim image');
CREATE TABLE IF NOT EXISTS querying (
	id SERIAL PRIMARY KEY,
	default_query_datetime TIMESTAMP NOT NULL,
	query_datetime TIMESTAMP,
	next_limited_query_datetime TIMESTAMP
);
CREATE TABLE IF NOT EXISTS queued_tweets (
	request_tweet_id VARCHAR(30) PRIMARY KEY,
	request_screen_name VARCHAR(30) NOT NULL,
	render_type render_type NOT NULL,
	render_tweet_id VARCHAR(30),
	text_ TEXT,
	image_info_source VARCHAR(50),
	image_info_profile_picture_url TEXT,
	image_info_datetime TIMESTAMP,
	image_info_verified BOOLEAN,
	image_info_protected BOOLEAN,
	image_info_name VARCHAR(50),
	image_info_screen_name VARCHAR(30)
);