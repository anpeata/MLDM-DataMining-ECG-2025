SELECT tablename FROM pg_tables WHERE schemaname = 'public';

select count(*) from scdhd;

select * from scdhd;

DELETE FROM scdhd;

CREATE TABLE scdhd (
    type TEXT,
    "0_pre-RR" NUMERIC,
    "0_post-RR" NUMERIC,
    "0_pPeak" NUMERIC,
    "0_tPeak" NUMERIC,
    "0_rPeak" NUMERIC,
    "0_sPeak" NUMERIC,
    "0_qPeak" NUMERIC,
    "0_qrs_interval" NUMERIC,
    "0_pq_interval" NUMERIC,
    "0_qt_interval" NUMERIC,
    "0_st_interval" NUMERIC,
    "0_qrs_morph0" NUMERIC,
    "0_qrs_morph1" NUMERIC,
    "0_qrs_morph2" NUMERIC,
    "0_qrs_morph3" NUMERIC,
    "0_qrs_morph4" NUMERIC,
    "1_pre-RR" NUMERIC,
    "1_post-RR" NUMERIC,
    "1_pPeak" NUMERIC,
    "1_tPeak" NUMERIC,
    "1_rPeak" NUMERIC,
    "1_sPeak" NUMERIC,
    "1_qPeak" NUMERIC,
    "1_qrs_interval" NUMERIC,
    "1_pq_interval" NUMERIC,
    "1_qt_interval" NUMERIC,
    "1_st_interval" NUMERIC,
    "1_qrs_morph0" NUMERIC,
    "1_qrs_morph1" NUMERIC,
    "1_qrs_morph2" NUMERIC,
    "1_qrs_morph3" NUMERIC,
    "1_qrs_morph4" NUMERIC
);

ALTER TABLE scdhd
ADD COLUMN "0_pre-RR" NUMERIC,
ADD COLUMN "0_post-RR" NUMERIC,
ADD COLUMN "0_pPeak" NUMERIC,
ADD COLUMN "0_tPeak" NUMERIC,
ADD COLUMN "0_rPeak" NUMERIC,
ADD COLUMN "0_sPeak" NUMERIC,
ADD COLUMN "0_qPeak" NUMERIC,
ADD COLUMN "0_qrs_interval" NUMERIC,
ADD COLUMN "0_pq_interval" NUMERIC,
ADD COLUMN "0_qt_interval" NUMERIC,
ADD COLUMN "0_st_interval" NUMERIC,
ADD COLUMN "0_qrs_morph0" NUMERIC,
ADD COLUMN "0_qrs_morph1" NUMERIC,
ADD COLUMN "0_qrs_morph2" NUMERIC,
ADD COLUMN "0_qrs_morph3" NUMERIC,
ADD COLUMN "0_qrs_morph4" NUMERIC,
ADD COLUMN "1_pre-RR" NUMERIC,
ADD COLUMN "1_post-RR" NUMERIC,
ADD COLUMN "1_pPeak" NUMERIC,
ADD COLUMN "1_tPeak" NUMERIC,
ADD COLUMN "1_rPeak" NUMERIC,
ADD COLUMN "1_sPeak" NUMERIC,
ADD COLUMN "1_qPeak" NUMERIC,
ADD COLUMN "1_qrs_interval" NUMERIC,
ADD COLUMN "1_pq_interval" NUMERIC,
ADD COLUMN "1_qt_interval" NUMERIC,
ADD COLUMN "1_st_interval" NUMERIC,
ADD COLUMN "1_qrs_morph0" NUMERIC,
ADD COLUMN "1_qrs_morph1" NUMERIC,
ADD COLUMN "1_qrs_morph2" NUMERIC,
ADD COLUMN "1_qrs_morph3" NUMERIC,
ADD COLUMN "1_qrs_morph4" NUMERIC;

ALTER TABLE scdhd
ADD COLUMN type TEXT;


COPY scdhd (type, "0_pre-RR", "0_post-RR", "0_pPeak", "0_tPeak", "0_rPeak", "0_sPeak", "0_qPeak", "0_qrs_interval", "0_pq_interval", "0_qt_interval", "0_st_interval", "0_qrs_morph0", "0_qrs_morph1", "0_qrs_morph2", "0_qrs_morph3", "0_qrs_morph4", "1_pre-RR", "1_post-RR", "1_pPeak", "1_tPeak", "1_rPeak", "1_sPeak", "1_qPeak", "1_qrs_interval", "1_pq_interval", "1_qt_interval", "1_st_interval", "1_qrs_morph0", "1_qrs_morph1", "1_qrs_morph2", "1_qrs_morph3", "1_qrs_morph4") 
FROM 'D:\Documents\master\mldm\m1\semester-2\data-mining\data\Sudden-Cardiac-Death-Holter-Database - Copy.csv' 
DELIMITER ',' 
CSV HEADER;

ALTER TABLE scdhd ADD COLUMN id SERIAL PRIMARY KEY;

CREATE INDEX idx_new ON scdhd(id);

SELECT 
    type,
    COUNT(*) AS total_rows,
    MIN("0_pre-RR") AS min_pre_RR, MAX("0_pre-RR") AS max_pre_RR, AVG("0_pre-RR") AS avg_pre_RR, STDDEV("0_pre-RR") AS std_pre_RR,
    MIN("1_pre-RR") AS min_pre_RR_1, MAX("1_pre-RR") AS max_pre_RR_1, AVG("1_pre-RR") AS avg_pre_RR_1, STDDEV("1_pre-RR") AS std_pre_RR_1
FROM scdhd
GROUP BY type;


SELECT 
    type,
    corr("0_pPeak", "0_tPeak") AS correlation_pPeak_tPeak,
    corr("0_rPeak", "0_qt_interval") AS correlation_rPeak_qtInterval,
    corr("1_qPeak", "1_qrs_interval") AS correlation_qPeak_qrsInterval
FROM scdhd
GROUP BY type;


WITH z_scores AS (
    SELECT *, 
        (("0_pPeak" - AVG("0_pPeak") OVER()) / STDDEV("0_pPeak") OVER()) AS z_pPeak,
        (("0_rPeak" - AVG("0_rPeak") OVER()) / STDDEV("0_rPeak") OVER()) AS z_rPeak
    FROM scdhd
)
SELECT * FROM z_scores WHERE ABS(z_pPeak) > 3 OR ABS(z_rPeak) > 3;


SELECT type, "0_pre-RR",
       AVG("0_pre-RR") OVER (PARTITION BY type ORDER BY "0_pre-RR" ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS moving_avg_pre_RR
FROM scdhd;


SELECT feature_name, 
       MAX(feature_value) - MIN(feature_value) AS range, 
       STDDEV(feature_value) AS std_dev,
       VARIANCE(feature_value) AS variance
FROM (
    SELECT unnest(ARRAY['0_pre-RR', '0_post-RR', '0_pPeak', '0_qt_interval']) AS feature_name,
           unnest(ARRAY["0_pre-RR", "0_post-RR", "0_pPeak", "0_qt_interval"]) AS feature_value
    FROM scdhd
) subquery
GROUP BY feature_name
ORDER BY variance DESC;


WITH mean_std AS (
    SELECT 
        AVG("0_qrs_interval") AS mean_qrs_interval, STDDEV("0_qrs_interval") AS std_qrs_interval,
        AVG("0_qt_interval") AS mean_qt_interval, STDDEV("0_qt_interval") AS std_qt_interval
    FROM scdhd
)
SELECT type,
    ("0_qrs_interval" - mean_qrs_interval) / std_qrs_interval AS pca_component_1,
    ("0_qt_interval" - mean_qt_interval) / std_qt_interval AS pca_component_2
FROM scdhd, mean_std;
