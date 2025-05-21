
create table if not exists agent_logs_reports (
  id serial primary key,
  created_at timestamp default current_timestamp,
  e jsonb
);

