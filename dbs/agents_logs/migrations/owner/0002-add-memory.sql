
create table if not exists agent_conversations (
  id serial primary key,
  created_at timestamp default current_timestamp,
  
  root_conversation_id uuid not null,

  conversation_id uuid not null,
  agent_slug text not null,
  agent_execution_id uuid not null,

  parent_conversation_id uuid,
  parent_agent_slug text,
  parent_agent_execution_id uuid,

  pending_query jsonb not null,
  llm_history jsonb not null
);
