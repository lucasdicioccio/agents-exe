create table if not exists agent_logs_reports (
  id serial primary key,
  created_at timestamp default current_timestamp,
  e jsonb
);

grant usage on schema public to agents_log_inserter;
grant insert (e) on agent_logs_reports to agents_log_inserter;
grant usage, select on agent_logs_reports_id_seq to agents_log_inserter;

grant insert (e) on agent_logs_reports to agents_log_anon;
grant usage, select on agent_logs_reports_id_seq to agents_log_anon;
grant usage on schema public to agents_log_anon;
grant select on agent_logs_reports to agents_log_anon;
