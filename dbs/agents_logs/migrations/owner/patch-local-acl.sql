grant usage on schema public to agents_log_inserter;

-- migrate.after: 0001-init.sql
grant insert (e) on agent_logs_reports to agents_log_inserter;
grant usage, select on agent_logs_reports_id_seq to agents_log_inserter;
grant insert (e) on agent_logs_reports to agents_log_anon;
grant usage, select on agent_logs_reports_id_seq to agents_log_anon;
grant usage on schema public to agents_log_anon;
grant select on agent_logs_reports to agents_log_anon;

-- migrate.after: 0002-add-memory.sql

grant insert (root_conversation_id,conversation_id,agent_slug,agent_execution_id,parent_conversation_id, parent_agent_slug, parent_agent_execution_id, pending_query, llm_history) on agent_conversations to agents_log_inserter;
grant usage, select on agent_conversations_id_seq to agents_log_inserter;
grant insert (root_conversation_id,conversation_id,agent_slug,agent_execution_id,parent_conversation_id, parent_agent_slug, parent_agent_execution_id, pending_query, llm_history) on agent_conversations to agents_log_anon;
grant usage, select on agent_conversations_id_seq to agents_log_anon;
grant usage on schema public to agents_log_anon;
grant select on agent_conversations to agents_log_anon;
