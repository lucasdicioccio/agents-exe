-- ============================================================================
-- MonsterGame - A Pokemon-like Toy Game Schema
-- ----------------------------------------------------------------------------
-- This schema demonstrates a relational database design for a monster 
-- collection and battle game. It is designed to work well with PostgREST
-- for RESTful API access.
--
-- Tables: 10 core tables + 1 view
-- Features:
--   - Enum types for game constants
--   - Foreign key relationships with proper constraints
--   - JSON columns for flexible data
--   - Triggers for updated_at timestamps
--   - Row Level Security (RLS) setup for user isolation
--   - Indexes for common query patterns
-- ============================================================================

-- ----------------------------------------------------------------------------
-- Extensions
-- ----------------------------------------------------------------------------
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- ----------------------------------------------------------------------------
-- Enum Types
-- ----------------------------------------------------------------------------
CREATE TYPE element_type AS ENUM (
    'normal', 'fire', 'water', 'electric', 'grass', 'ice',
    'fighting', 'poison', 'ground', 'flying', 'psychic',
    'bug', 'rock', 'ghost', 'dragon', 'dark', 'steel', 'fairy'
);

CREATE TYPE item_category AS ENUM (
    'pokeball', 'potion', 'revive', 'status_heal', 'battle_item', 'key_item'
);

CREATE TYPE battle_status AS ENUM (
    'pending', 'active', 'completed', 'cancelled'
);

CREATE TYPE monster_rarity AS ENUM (
    'common', 'uncommon', 'rare', 'epic', 'legendary', 'mythical'
);

-- ----------------------------------------------------------------------------
-- Table 1: Trainers
-- ----------------------------------------------------------------------------
CREATE TABLE trainers (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    username TEXT NOT NULL UNIQUE CHECK (length(username) >= 3 AND length(username) <= 20),
    email TEXT UNIQUE,
    password_hash TEXT, -- Nullable for demo; would be required in production
    display_name TEXT,
    avatar_url TEXT,
    coins INTEGER NOT NULL DEFAULT 0 CHECK (coins >= 0),
    wins INTEGER NOT NULL DEFAULT 0 CHECK (wins >= 0),
    losses INTEGER NOT NULL DEFAULT 0 CHECK (losses >= 0),
    started_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    last_active_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    is_active BOOLEAN NOT NULL DEFAULT true,
    settings JSONB DEFAULT '{}'::jsonb, -- UI preferences, notifications, etc.
    
    CONSTRAINT valid_email CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$')
);

COMMENT ON TABLE trainers IS 'Player accounts who collect and battle monsters';
COMMENT ON COLUMN trainers.coins IS 'In-game currency for purchasing items';
COMMENT ON COLUMN trainers.settings IS 'Player preferences stored as JSON for flexibility';

-- ----------------------------------------------------------------------------
-- Table 2: Monster Species (Templates)
-- ----------------------------------------------------------------------------
CREATE TABLE monster_species (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    description TEXT,
    primary_type element_type NOT NULL,
    secondary_type element_type,
    rarity monster_rarity NOT NULL DEFAULT 'common',
    
    -- Base stats (0-255 scale like Pokemon)
    base_hp INTEGER NOT NULL CHECK (base_hp BETWEEN 1 AND 255),
    base_attack INTEGER NOT NULL CHECK (base_attack BETWEEN 1 AND 255),
    base_defense INTEGER NOT NULL CHECK (base_defense BETWEEN 1 AND 255),
    base_sp_attack INTEGER NOT NULL CHECK (base_sp_attack BETWEEN 1 AND 255),
    base_sp_defense INTEGER NOT NULL CHECK (base_sp_defense BETWEEN 1 AND 255),
    base_speed INTEGER NOT NULL CHECK (base_speed BETWEEN 1 AND 255),
    
    catch_rate INTEGER NOT NULL CHECK (catch_rate BETWEEN 1 AND 255),
    xp_yield INTEGER NOT NULL CHECK (xp_yield >= 0),
    evolutions JSONB DEFAULT '[]'::jsonb, -- Array of {species_id, level_required, item_required}
    sprite_url TEXT,
    
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

COMMENT ON TABLE monster_species IS 'Base templates defining monster types, stats, and evolution paths';
COMMENT ON COLUMN monster_species.evolutions IS 'JSON array describing evolution requirements';

-- ----------------------------------------------------------------------------
-- Table 3: Individual Monsters
-- ----------------------------------------------------------------------------
CREATE TABLE monsters (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    species_id INTEGER NOT NULL REFERENCES monster_species(id) ON DELETE RESTRICT,
    trainer_id UUID REFERENCES trainers(id) ON DELETE SET NULL, -- NULL = wild
    
    nickname TEXT CHECK (nickname IS NULL OR length(nickname) <= 20),
    level INTEGER NOT NULL DEFAULT 1 CHECK (level BETWEEN 1 AND 100),
    xp INTEGER NOT NULL DEFAULT 0 CHECK (xp >= 0),
    
    -- Individual Values (IVs) - genetic variance 0-31
    iv_hp INTEGER NOT NULL CHECK (iv_hp BETWEEN 0 AND 31) DEFAULT floor(random() * 32)::int,
    iv_attack INTEGER NOT NULL CHECK (iv_attack BETWEEN 0 AND 31) DEFAULT floor(random() * 32)::int,
    iv_defense INTEGER NOT NULL CHECK (iv_defense BETWEEN 0 AND 31) DEFAULT floor(random() * 32)::int,
    iv_sp_attack INTEGER NOT NULL CHECK (iv_sp_attack BETWEEN 0 AND 31) DEFAULT floor(random() * 32)::int,
    iv_sp_defense INTEGER NOT NULL CHECK (iv_sp_defense BETWEEN 0 AND 31) DEFAULT floor(random() * 32)::int,
    iv_speed INTEGER NOT NULL CHECK (iv_speed BETWEEN 0 AND 31) DEFAULT floor(random() * 32)::int,
    
    -- Effort Values (EVs) - training bonuses, max 510 total, 255 per stat
    ev_hp INTEGER NOT NULL DEFAULT 0 CHECK (ev_hp BETWEEN 0 AND 255),
    ev_attack INTEGER NOT NULL DEFAULT 0 CHECK (ev_attack BETWEEN 0 AND 255),
    ev_defense INTEGER NOT NULL DEFAULT 0 CHECK (ev_defense BETWEEN 0 AND 255),
    ev_sp_attack INTEGER NOT NULL DEFAULT 0 CHECK (ev_sp_attack BETWEEN 0 AND 255),
    ev_sp_defense INTEGER NOT NULL DEFAULT 0 CHECK (ev_sp_defense BETWEEN 0 AND 255),
    ev_speed INTEGER NOT NULL DEFAULT 0 CHECK (ev_speed BETWEEN 0 AND 255),
    
    current_hp INTEGER NOT NULL,
    is_fainted BOOLEAN NOT NULL DEFAULT false,
    status_condition TEXT CHECK (status_condition IN ('none', 'poison', 'burn', 'paralysis', 'sleep', 'freeze')),
    held_item_id INTEGER, -- References items, added later
    
    caught_at TIMESTAMPTZ DEFAULT now(),
    caught_location TEXT DEFAULT 'Unknown',
    
    CONSTRAINT valid_evs CHECK (ev_hp + ev_attack + ev_defense + ev_sp_attack + ev_sp_defense + ev_speed <= 510),
    CONSTRAINT current_hp_not_negative CHECK (current_hp >= 0)
);

COMMENT ON TABLE monsters IS 'Individual monster instances owned by trainers or roaming wild';
COMMENT ON COLUMN monsters.iv_hp IS 'Individual Value: genetic potential (0-31)';
COMMENT ON COLUMN monsters.ev_hp IS 'Effort Value: training bonus (0-255 per stat, 510 max total)';

-- ----------------------------------------------------------------------------
-- Table 4: Moves
-- ----------------------------------------------------------------------------
CREATE TABLE moves (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    description TEXT,
    type element_type NOT NULL,
    category TEXT NOT NULL CHECK (category IN ('physical', 'special', 'status')),
    power INTEGER CHECK (power IS NULL OR power BETWEEN 0 AND 250),
    accuracy INTEGER CHECK (accuracy IS NULL OR accuracy BETWEEN 0 AND 100),
    pp INTEGER NOT NULL CHECK (pp BETWEEN 1 AND 40),
    priority INTEGER NOT NULL DEFAULT 0 CHECK (priority BETWEEN -7 AND 7),
    effect JSONB DEFAULT '{}'::jsonb, -- Status effects, stat changes, etc.
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

COMMENT ON TABLE moves IS 'Attacks and abilities monsters can learn';
COMMENT ON COLUMN moves.effect IS 'JSON describing additional effects beyond damage';

-- ----------------------------------------------------------------------------
-- Table 5: Monster Moves (Junction)
-- ----------------------------------------------------------------------------
CREATE TABLE monster_moves (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    monster_id UUID NOT NULL REFERENCES monsters(id) ON DELETE CASCADE,
    move_id INTEGER NOT NULL REFERENCES moves(id) ON DELETE RESTRICT,
    current_pp INTEGER NOT NULL,
    is_learned BOOLEAN NOT NULL DEFAULT true, -- vs. temporary (e.g., via TM)
    learned_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    slot_number INTEGER NOT NULL CHECK (slot_number BETWEEN 1 AND 4),
    
    UNIQUE(monster_id, slot_number)
);

COMMENT ON TABLE monster_moves IS 'Moves known by individual monsters (max 4 per monster)';

-- ----------------------------------------------------------------------------
-- Table 6: Items
-- ----------------------------------------------------------------------------
CREATE TABLE items (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    description TEXT,
    category item_category NOT NULL,
    price INTEGER NOT NULL DEFAULT 0 CHECK (price >= 0),
    effect JSONB DEFAULT '{}'::jsonb, -- Healing amount, catch modifier, etc.
    icon_url TEXT,
    is_usable_in_battle BOOLEAN NOT NULL DEFAULT true,
    is_consumable BOOLEAN NOT NULL DEFAULT true,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

COMMENT ON TABLE items IS 'Items that can be purchased, found, or held by monsters';

-- Add foreign key to monsters for held items now that items exists
ALTER TABLE monsters 
    ADD CONSTRAINT fk_held_item 
    FOREIGN KEY (held_item_id) REFERENCES items(id) ON DELETE SET NULL;

-- ----------------------------------------------------------------------------
-- Table 7: Inventory (Trainer Items)
-- ----------------------------------------------------------------------------
CREATE TABLE inventory (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    trainer_id UUID NOT NULL REFERENCES trainers(id) ON DELETE CASCADE,
    item_id INTEGER NOT NULL REFERENCES items(id) ON DELETE RESTRICT,
    quantity INTEGER NOT NULL DEFAULT 1 CHECK (quantity >= 0),
    acquired_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    acquired_from TEXT DEFAULT 'purchase', -- 'purchase', 'found', 'gift', 'battle'
    
    UNIQUE(trainer_id, item_id)
);

COMMENT ON TABLE inventory IS 'Items owned by trainers with quantities';

-- ----------------------------------------------------------------------------
-- Table 8: Type Matchups (Type Effectiveness)
-- ----------------------------------------------------------------------------
CREATE TABLE type_matchups (
    id SERIAL PRIMARY KEY,
    attacking_type element_type NOT NULL,
    defending_type element_type NOT NULL,
    effectiveness NUMERIC(3,2) NOT NULL CHECK (effectiveness IN (0, 0.25, 0.5, 1, 2, 4)),
    
    UNIQUE(attacking_type, defending_type)
);

COMMENT ON TABLE type_matchups IS 'Damage multipliers for type interactions (e.g., Water vs Fire = 2.0)';

-- ----------------------------------------------------------------------------
-- Table 9: Species Moves (Learnset)
-- ----------------------------------------------------------------------------
CREATE TABLE species_moves (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    species_id INTEGER NOT NULL REFERENCES monster_species(id) ON DELETE CASCADE,
    move_id INTEGER NOT NULL REFERENCES moves(id) ON DELETE CASCADE,
    learned_at_level INTEGER CHECK (learned_at_level IS NULL OR learned_at_level BETWEEN 1 AND 100),
    learned_by TEXT DEFAULT 'level_up' CHECK (learned_by IN ('level_up', 'tm', 'tutor', 'egg')),
    
    UNIQUE(species_id, move_id, learned_by)
);

COMMENT ON TABLE species_moves IS 'Moves each species can learn and how they learn them';

-- ----------------------------------------------------------------------------
-- Table 10: Battles
-- ----------------------------------------------------------------------------
CREATE TABLE battles (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    status battle_status NOT NULL DEFAULT 'pending',
    
    trainer_1_id UUID NOT NULL REFERENCES trainers(id) ON DELETE CASCADE,
    trainer_2_id UUID REFERENCES trainers(id) ON DELETE CASCADE, -- NULL = wild battle
    
    winner_id UUID REFERENCES trainers(id) ON DELETE SET NULL,
    
    started_at TIMESTAMPTZ,
    ended_at TIMESTAMPTZ,
    
    battle_type TEXT NOT NULL DEFAULT 'single' CHECK (battle_type IN ('single', 'double', 'wild')),
    weather TEXT DEFAULT 'clear' CHECK (weather IN ('clear', 'rain', 'sun', 'sand', 'hail')),
    
    rounds INTEGER DEFAULT 0,
    log JSONB DEFAULT '[]'::jsonb, -- Turn-by-turn log for replay
    
    CONSTRAINT different_trainers CHECK (trainer_1_id != trainer_2_id)
);

COMMENT ON TABLE battles IS 'Battle records between trainers or against wild monsters';
COMMENT ON COLUMN battles.log IS 'JSON array of turn events for battle replay';

-- ----------------------------------------------------------------------------
-- View: Trainer Statistics
-- ----------------------------------------------------------------------------
CREATE VIEW trainer_statistics AS
SELECT 
    t.id AS trainer_id,
    t.username,
    t.display_name,
    t.coins,
    t.wins,
    t.losses,
    CASE 
        WHEN (t.wins + t.losses) > 0 
        THEN ROUND(100.0 * t.wins / (t.wins + t.losses), 2)
        ELSE 0 
    END AS win_rate,
    COUNT(DISTINCT m.id) AS monster_count,
    COUNT(DISTINCT CASE WHEN m.is_fainted = false THEN m.id END) AS healthy_monsters,
    COALESCE(SUM(i.quantity), 0) AS total_items,
    t.started_at,
    t.last_active_at
FROM trainers t
LEFT JOIN monsters m ON m.trainer_id = t.id
LEFT JOIN inventory i ON i.trainer_id = t.id
GROUP BY t.id, t.username, t.display_name, t.coins, t.wins, t.losses, t.started_at, t.last_active_at;

COMMENT ON VIEW trainer_statistics IS 'Aggregated view of trainer performance and collection status';

-- ----------------------------------------------------------------------------
-- Indexes for Performance
-- ----------------------------------------------------------------------------
CREATE INDEX idx_monsters_trainer ON monsters(trainer_id);
CREATE INDEX idx_monsters_species ON monsters(species_id);
CREATE INDEX idx_monsters_level ON monsters(level DESC);
CREATE INDEX idx_monster_moves_monster ON monster_moves(monster_id);
CREATE INDEX idx_inventory_trainer ON inventory(trainer_id);
CREATE INDEX idx_battles_trainer1 ON battles(trainer_1_id);
CREATE INDEX idx_battles_trainer2 ON battles(trainer_2_id);
CREATE INDEX idx_battles_status ON battles(status);
CREATE INDEX idx_species_type ON monster_species(primary_type);
CREATE INDEX idx_moves_type ON moves(type);

-- GIN index for JSON queries
CREATE INDEX idx_monsters_evolutions ON monster_species USING GIN(evolutions);
CREATE INDEX idx_items_effect ON items USING GIN(effect);

-- ----------------------------------------------------------------------------
-- Helper Functions
-- ----------------------------------------------------------------------------

-- Calculate actual stat value based on base, IV, EV, and level
CREATE OR REPLACE FUNCTION calculate_stat(
    base_stat INTEGER,
    iv INTEGER,
    ev INTEGER,
    level INTEGER,
    is_hp BOOLEAN DEFAULT false
) RETURNS INTEGER AS $$
BEGIN
    IF is_hp THEN
        RETURN floor(((((2 * base_stat + iv + floor(ev/4)) * level) / 100) + level + 10))::int;
    ELSE
        RETURN floor(((((2 * base_stat + iv + floor(ev/4)) * level) / 100) + 5)))::int;
    END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-- Trigger to update last_active_at
CREATE OR REPLACE FUNCTION update_trainer_last_active()
RETURNS TRIGGER AS $$
BEGIN
    NEW.last_active_at = now();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_trainer_last_active
    BEFORE UPDATE ON trainers
    FOR EACH ROW
    EXECUTE FUNCTION update_trainer_last_active();

-- Trigger to set current_hp when monster is created (based on calculated max HP)
CREATE OR REPLACE FUNCTION set_monster_initial_hp()
RETURNS TRIGGER AS $$
DECLARE
    base_hp INTEGER;
    max_hp INTEGER;
BEGIN
    SELECT ms.base_hp INTO base_hp FROM monster_species ms WHERE ms.id = NEW.species_id;
    max_hp := calculate_stat(base_hp, NEW.iv_hp, NEW.ev_hp, NEW.level, true);
    NEW.current_hp = max_hp;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_set_monster_hp
    BEFORE INSERT ON monsters
    FOR EACH ROW
    EXECUTE FUNCTION set_monster_initial_hp();

-- Trigger to set initial PP for moves
CREATE OR REPLACE FUNCTION set_move_initial_pp()
RETURNS TRIGGER AS $$
DECLARE
    move_pp INTEGER;
BEGIN
    SELECT pp INTO move_pp FROM moves WHERE id = NEW.move_id;
    NEW.current_pp = move_pp;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_set_move_pp
    BEFORE INSERT ON monster_moves
    FOR EACH ROW
    EXECUTE FUNCTION set_move_initial_pp();

-- ----------------------------------------------------------------------------
-- Row Level Security (RLS) Policies
-- ----------------------------------------------------------------------------
ALTER TABLE trainers ENABLE ROW LEVEL SECURITY;
ALTER TABLE monsters ENABLE ROW LEVEL SECURITY;
ALTER TABLE inventory ENABLE ROW LEVEL SECURITY;

-- Trainers can see all trainers (for leaderboards) but only edit themselves
CREATE POLICY trainers_select_all ON trainers FOR SELECT USING (true);
CREATE POLICY trainers_update_own ON trainers FOR UPDATE USING (id = current_setting('app.current_user_id')::uuid);

-- Trainers can see all monsters but only modify their own
CREATE POLICY monsters_select_all ON monsters FOR SELECT USING (true);
CREATE POLICY monsters_modify_own ON monsters FOR ALL USING (trainer_id = current_setting('app.current_user_id')::uuid);

-- Inventory is private per trainer
CREATE POLICY inventory_select_own ON inventory FOR SELECT USING (trainer_id = current_setting('app.current_user_id')::uuid);
CREATE POLICY inventory_modify_own ON inventory FOR ALL USING (trainer_id = current_setting('app.current_user_id')::uuid);

-- =============================================================================
-- SAMPLE DATA INSERTION
-- =============================================================================

-- ----------------------------------------------------------------------------
-- Sample Trainers
-- ----------------------------------------------------------------------------
INSERT INTO trainers (id, username, email, display_name, coins, wins, losses, settings) VALUES
    ('550e8400-e29b-41d4-a716-446655440000', 'red_ketchum', 'red@example.com', 'Red', 5000, 42, 8, '{"theme": "classic", "notifications": true}'),
    ('550e8400-e29b-41d4-a716-446655440001', 'misty_water', 'misty@example.com', 'Misty', 3200, 28, 12, '{"theme": "water", "favorite_type": "water"}'),
    ('550e8400-e29b-41d4-a716-446655440002', 'brock_rock', 'brock@example.com', 'Brock', 2800, 35, 15, '{"theme": "dark", "favorite_type": "rock"}'),
    ('550e8400-e29b-41d4-a716-446655440003', 'green_oak', 'green@example.com', 'Green', 4500, 38, 10, '{"theme": "classic", "rival": "red"}'),
    ('550e8400-e29b-41d4-a716-446655440004', 'prof_elm', 'elm@example.com', 'Professor Elm', 10000, 5, 2, '{"theme": "lab", "is_professor": true}');

-- ----------------------------------------------------------------------------
-- Sample Monster Species
-- ----------------------------------------------------------------------------
INSERT INTO monster_species (id, name, description, primary_type, secondary_type, rarity, base_hp, base_attack, base_defense, base_sp_attack, base_sp_defense, base_speed, catch_rate, xp_yield, evolutions, sprite_url) VALUES
    (1, 'Flametoad', 'A small fire toad that thrives in volcanic regions.', 'fire', NULL, 'common', 39, 52, 43, 60, 50, 65, 45, 62, '[{"species_id": 2, "level_required": 16}]', '/sprites/flametoad.png'),
    (2, 'Infernap', 'The evolved form of Flametoad. Its flames burn hotter.', 'fire', NULL, 'uncommon', 58, 64, 58, 80, 65, 80, 45, 142, '[{"species_id": 3, "level_required": 36}]', '/sprites/infernap.png'),
    (3, 'Volcanicore', 'A massive fire beast that erupts like a volcano.', 'fire', 'ground', 'rare', 78, 84, 78, 109, 85, 100, 45, 240, '[]', '/sprites/volcanicore.png'),
    (4, 'Aquaduck', 'A water bird that lives near lakes and rivers.', 'water', NULL, 'common', 44, 48, 65, 50, 64, 43, 45, 63, '[{"species_id": 5, "level_required": 16}]', '/sprites/aquaduck.png'),
    (5, 'Aquafowl', 'Elegant water fowl with powerful hydro pumps.', 'water', NULL, 'uncommon', 59, 63, 80, 65, 80, 58, 45, 142, '[{"species_id": 6, "level_required": 36}]', '/sprites/aquafowl.png'),
    (6, 'Hydroking', 'The emperor of water monsters. Its shell is indestructible.', 'water', 'steel', 'rare', 79, 83, 100, 85, 105, 78, 45, 239, '[]', '/sprites/hydroking.png'),
    (7, 'Sparkrat', 'An electric rodent that stores electricity in its cheeks.', 'electric', NULL, 'common', 35, 55, 40, 50, 50, 90, 190, 112, '[{"species_id": 8, "level_required": 25}]', '/sprites/sparkrat.png'),
    (8, 'Voltra', 'Evolved electric rodent. Thunder follows where it runs.', 'electric', NULL, 'uncommon', 60, 90, 55, 90, 80, 110, 75, 218, '[]', '/sprites/voltra.png'),
    (9, 'Bulbroot', 'A grass monster with a seed on its back.', 'grass', 'poison', 'common', 45, 49, 49, 65, 65, 45, 45, 64, '[{"species_id": 10, "level_required": 16}]', '/sprites/bulbroot.png'),
    (10, 'Thornvine', 'The seed has grown into a blooming flower.', 'grass', 'poison', 'uncommon', 60, 62, 63, 80, 80, 60, 45, 142, '[{"species_id": 11, "level_required": 32}]', '/sprites/thornvine.png'),
    (11, 'Megabloom', 'The flower has fully bloomed, releasing sweet toxic pollen.', 'grass', 'poison', 'rare', 80, 82, 83, 100, 100, 80, 45, 236, '[]', '/sprites/megabloom.png'),
    (12, 'Dragling', 'A young dragon with tremendous potential.', 'dragon', NULL, 'legendary', 41, 64, 45, 50, 50, 50, 45, 67, '[{"species_id": 13, "level_required": 30}]', '/sprites/dragling.png'),
    (13, 'Wyrmolt', 'A mid-stage dragon with developing wings.', 'dragon', NULL, 'legendary', 61, 84, 65, 70, 70, 70, 45, 147, '[{"species_id": 14, "level_required": 55}]', '/sprites/wyrmolt.png'),
    (14, 'Dracolord', 'An ancient dragon of immense power.', 'dragon', 'flying', 'mythical', 91, 134, 95, 100, 100, 80, 45, 306, '[]', '/sprites/dracolord.png');

-- ----------------------------------------------------------------------------
-- Sample Moves
-- ----------------------------------------------------------------------------
INSERT INTO moves (id, name, description, type, category, power, accuracy, pp, priority, effect) VALUES
    (1, 'Tackle', 'A physical attack in which the user charges and slams into the target.', 'normal', 'physical', 40, 100, 35, 0, '{}'),
    (2, 'Ember', 'The target is attacked with small flames.', 'fire', 'special', 40, 100, 25, 0, '{"burn_chance": 10}'),
    (3, 'Flamethrower', 'The target is scorched with an intense blast of fire.', 'fire', 'special', 90, 100, 15, 0, '{"burn_chance": 10}'),
    (4, 'Water Gun', 'The target is blasted with a forceful shot of water.', 'water', 'special', 40, 100, 25, 0, '{}'),
    (5, 'Hydro Pump', 'The target is blasted by a huge volume of water launched under great pressure.', 'water', 'special', 110, 80, 5, 0, '{}'),
    (6, 'Thunder Shock', 'A jolt of electricity crashes down on the target.', 'electric', 'special', 40, 100, 30, 0, '{"paralysis_chance": 10}'),
    (7, 'Thunderbolt', 'A strong electric blast crashes down on the target.', 'electric', 'special', 90, 100, 15, 0, '{"paralysis_chance": 10}'),
    (8, 'Vine Whip', 'The target is struck with slender, whiplike vines.', 'grass', 'physical', 45, 100, 25, 0, '{}'),
    (9, 'Solar Beam', 'A two-turn attack. The user gathers light, then blasts a bundled beam.', 'grass', 'special', 120, 100, 10, 0, '{"charge_turn": true}'),
    (10, 'Growl', 'The user growls in an endearing way, making the opposing team less wary.', 'normal', 'status', NULL, 100, 40, 0, '{"stat_change": {"attack": -1}}'),
    (11, 'Dragon Claw', 'The user slashes the target with huge sharp claws.', 'dragon', 'physical', 80, 100, 15, 0, '{}'),
    (12, 'Dragon Breath', 'The user exhales a mighty gust that inflicts damage.', 'dragon', 'special', 60, 100, 20, 0, '{"paralysis_chance": 30}'),
    (13, 'Quick Attack', 'The user lunges at the target at a speed that makes it almost invisible.', 'normal', 'physical', 40, 100, 30, 1, '{}'),
    (14, 'Growl', 'Lowers the target''s Attack stat.', 'normal', 'status', NULL, 100, 40, 0, '{"stat_changes": [{"stat": "attack", "stages": -1}]}'),
    (15, 'Tail Whip', 'The user wags its tail cutely, making opposing monsters less wary.', 'normal', 'status', NULL, 100, 30, 0, '{"stat_changes": [{"stat": "defense", "stages": -1}]}');

-- ----------------------------------------------------------------------------
-- Sample Species Moves (Learnsets)
-- ----------------------------------------------------------------------------
INSERT INTO species_moves (species_id, move_id, learned_at_level, learned_by) VALUES
    (1, 1, 1, 'level_up'),    -- Flametoad: Tackle
    (1, 2, 5, 'level_up'),    -- Flametoad: Ember
    (1, 3, 30, 'level_up'),   -- Flametoad: Flamethrower
    (2, 3, 34, 'level_up'),   -- Infernap: Flamethrower
    (3, 3, 34, 'level_up'),   -- Volcanicore: Flamethrower
    (4, 1, 1, 'level_up'),    -- Aquaduck: Tackle
    (4, 4, 5, 'level_up'),    -- Aquaduck: Water Gun
    (4, 5, 42, 'level_up'),   -- Aquaduck: Hydro Pump
    (5, 5, 47, 'level_up'),   -- Aquafowl: Hydro Pump
    (6, 5, 47, 'level_up'),   -- Hydroking: Hydro Pump
    (7, 1, 1, 'level_up'),    -- Sparkrat: Tackle
    (7, 6, 5, 'level_up'),    -- Sparkrat: Thunder Shock
    (7, 7, 26, 'level_up'),   -- Sparkrat: Thunderbolt
    (8, 7, 42, 'level_up'),   -- Voltra: Thunderbolt
    (9, 8, 5, 'level_up'),    -- Bulbroot: Vine Whip
    (9, 9, 46, 'level_up'),   -- Bulbroot: Solar Beam
    (10, 9, 54, 'level_up'),  -- Thornvine: Solar Beam
    (11, 9, 65, 'level_up'),  -- Megabloom: Solar Beam
    (12, 11, 20, 'level_up'), -- Dragling: Dragon Claw
    (12, 12, 1, 'level_up'),  -- Dragling: Dragon Breath
    (13, 11, 45, 'level_up'), -- Wyrmolt: Dragon Claw
    (14, 11, 75, 'level_up'), -- Dracolord: Dragon Claw
    (14, 12, 1, 'level_up');  -- Dracolord: Dragon Breath

-- ----------------------------------------------------------------------------
-- Sample Items
-- ----------------------------------------------------------------------------
INSERT INTO items (id, name, description, category, price, effect, is_usable_in_battle, is_consumable) VALUES
    (1, 'Potion', 'Restores 20 HP to one monster.', 'potion', 300, '{"heal_amount": 20}', true, true),
    (2, 'Super Potion', 'Restores 50 HP to one monster.', 'potion', 700, '{"heal_amount": 50}', true, true),
    (3, 'Hyper Potion', 'Restores 200 HP to one monster.', 'potion', 1500, '{"heal_amount": 200}', true, true),
    (4, 'Revive', 'Revives a fainted monster with half HP.', 'revive', 2000, '{"revive": true, "hp_percent": 50}', true, true),
    (5, 'Poke Ball', 'A device for catching wild monsters.', 'pokeball', 200, '{"catch_rate_multiplier": 1.0}', true, true),
    (6, 'Great Ball', 'A good, high-performance ball.', 'pokeball', 600, '{"catch_rate_multiplier": 1.5}', true, true),
    (7, 'Ultra Ball', 'An ultra-high performance ball.', 'pokeball', 1200, '{"catch_rate_multiplier": 2.0}', true, true),
    (8, 'Master Ball', 'The best ball. It never misses.', 'pokeball', 0, '{"catch_rate_multiplier": 255.0}', true, true),
    (9, 'Paralyze Heal', 'Heals paralysis from one monster.', 'status_heal', 200, '{"cures": "paralysis"}', true, true),
    (10, 'Burn Heal', 'Heals a burn from one monster.', 'status_heal', 250, '{"cures": "burn"}', true, true),
    (11, 'X Attack', 'Raises Attack stat in battle.', 'battle_item', 500, '{"stat_boost": {"attack": 1}}', true, true),
    (12, 'Bicycle', 'A folding bicycle that is faster than running.', 'key_item', 0, '{"effect": "faster_movement"}', false, false);

-- ----------------------------------------------------------------------------
-- Sample Inventory
-- ----------------------------------------------------------------------------
INSERT INTO inventory (trainer_id, item_id, quantity) VALUES
    ('550e8400-e29b-41d4-a716-446655440000', 1, 10),  -- Red: Potions
    ('550e8400-e29b-41d4-a716-446655440000', 5, 50),  -- Red: Poke Balls
    ('550e8400-e29b-41d4-a716-446655440000', 6, 20),  -- Red: Great Balls
    ('550e8400-e29b-41d4-a716-446655440000', 12, 1),  -- Red: Bicycle
    ('550e8400-e29b-41d4-a716-446655440001', 2, 15),  -- Misty: Super Potions
    ('550e8400-e29b-41d4-a716-446655440001', 7, 10),  -- Misty: Ultra Balls
    ('550e8400-e29b-41d4-a716-446655440002', 3, 20),  -- Brock: Hyper Potions
    ('550e8400-e29b-41d4-a716-446655440002', 4, 5),   -- Brock: Revives
    ('550e8400-e29b-41d4-a716-446655440003', 5, 30),  -- Green: Poke Balls
    ('550e8400-e29b-41d4-a716-446655440003', 8, 1),   -- Green: Master Ball
    ('550e8400-e29b-41d4-a716-446655440004', 1, 99),  -- Professor: Everything
    ('550e8400-e29b-41d4-a716-446655440004', 2, 99),
    ('550e8400-e29b-41d4-a716-446655440004', 3, 99),
    ('550e8400-e29b-41d4-a716-446655440004', 4, 99),
    ('550e8400-e29b-41d4-a716-446655440004', 5, 99),
    ('550e8400-e29b-41d4-a716-446655440004', 6, 99),
    ('550e8400-e29b-41d4-a716-446655440004', 7, 99);

-- ----------------------------------------------------------------------------
-- Sample Monsters
-- ----------------------------------------------------------------------------
INSERT INTO monsters (id, species_id, trainer_id, nickname, level, xp, iv_hp, iv_attack, iv_defense, iv_sp_attack, iv_sp_defense, iv_speed, current_hp, is_fainted, status_condition, held_item_id, caught_location) VALUES
    -- Red's team
    ('660e8400-e29b-41d4-a716-446655440000', 3, '550e8400-e29b-41d4-a716-446655440000', 'Blaze', 65, 245000, 31, 30, 28, 31, 25, 29, 210, false, 'none', NULL, 'Victory Road'),
    ('660e8400-e29b-41d4-a716-446655440001', 6, '550e8400-e29b-41d4-a716-446655440000', 'Tsunami', 62, 220000, 28, 25, 30, 29, 31, 27, 195, false, 'none', NULL, 'Seafoam Islands'),
    ('660e8400-e29b-41d4-a716-446655440002', 8, '550e8400-e29b-41d4-a716-446655440000', 'Sparky', 60, 200000, 29, 31, 26, 30, 28, 31, 170, false, 'none', NULL, 'Power Plant'),
    ('660e8400-e29b-41d4-a716-446655440003', 14, '550e8400-e29b-41d4-a716-446655440000', 'Draco', 70, 300000, 30, 31, 28, 31, 29, 27, 260, false, 'none', NULL, 'Dragon Cave'),
    ('660e8400-e29b-41d4-a716-446655440004', 11, '550e8400-e29b-41d4-a716-446655440000', 'Venus', 63, 230000, 27, 28, 30, 31, 30, 25, 200, false, 'none', NULL, 'Viridian Forest'),
    ('660e8400-e29b-41d4-a716-446655440005', 2, '550e8400-e29b-41d4-a716-446655440000', NULL, 45, 85000, 20, 22, 18, 25, 21, 24, 130, false, 'none', NULL, 'Route 1'),
    
    -- Misty's team
    ('660e8400-e29b-41d4-a716-446655440006', 6, '550e8400-e29b-41d4-a716-446655440001', 'Bubbles', 58, 180000, 25, 28, 30, 26, 31, 24, 190, false, 'none', 3, 'Cerulean Gym'),
    ('660e8400-e29b-41d4-a716-446655440007', 5, '550e8400-e29b-41d4-a716-446655440001', NULL, 45, 90000, 22, 24, 26, 23, 25, 22, 140, false, 'none', NULL, 'Route 4'),
    ('660e8400-e29b-41d4-a716-446655440008', 5, '550e8400-e29b-41d4-a716-446655440001', NULL, 42, 75000, 20, 21, 24, 22, 23, 21, 125, false, 'none', NULL, 'Route 25'),
    
    -- Brock's team
    ('660e8400-e29b-41d4-a716-446655440009', 2, '550e8400-e29b-41d4-a716-446655440002', 'Magma', 50, 110000, 24, 30, 28, 26, 24, 25, 155, false, 'none', NULL, 'Mt. Moon'),
    ('660e8400-e29b-41d4-a716-446655440010', 2, '550e8400-e29b-41d4-a716-446655440002', NULL, 48, 100000, 23, 28, 27, 25, 23, 24, 145, false, 'none', NULL, 'Pewter Gym'),
    
    -- Green's team
    ('660e8400-e29b-41d4-a716-446655440011', 6, '550e8400-e29b-41d4-a716-446655440003', NULL, 60, 200000, 28, 27, 29, 28, 30, 26, 195, false, 'none', NULL, 'Pallet Town'),
    ('660e8400-e29b-41d4-a716-446655440012', 3, '550e8400-e29b-41d4-a716-446655440003', NULL, 60, 200000, 27, 29, 26, 28, 27, 28, 200, false, 'none', NULL, 'Cinnabar Island'),
    
    -- Some wild monsters (NULL trainer_id)
    ('660e8400-e29b-41d4-a716-446655440013', 1, NULL, NULL, 5, 100, 10, 12, 8, 15, 11, 14, 20, false, 'none', NULL, 'Route 1'),
    ('660e8400-e29b-41d4-a716-446655440014', 4, NULL, NULL, 6, 120, 14, 10, 12, 13, 12, 11, 24, false, 'none', NULL, 'Route 22'),
    ('660e8400-e29b-41d4-a716-446655440015', 7, NULL, NULL, 4, 85, 8, 14, 10, 16, 13, 15, 18, false, 'none', NULL, 'Viridian Forest'),
    ('660e8400-e29b-41d4-a716-446655440016', 9, NULL, NULL, 5, 100, 12, 11, 13, 14, 15, 10, 21, false, 'none', NULL, 'Route 2');

-- ----------------------------------------------------------------------------
-- Sample Monster Moves
-- ----------------------------------------------------------------------------
INSERT INTO monster_moves (monster_id, move_id, slot_number) VALUES
    ('660e8400-e29b-41d4-a716-446655440000', 3, 1),   -- Blaze: Flamethrower
    ('660e8400-e29b-41d4-a716-446655440000', 13, 2),  -- Blaze: Quick Attack
    ('660e8400-e29b-41d4-a716-446655440000', 11, 3),  -- Blaze: Dragon Claw
    ('660e8400-e29b-41d4-a716-446655440000', 10, 4),  -- Blaze: Growl
    
    ('660e8400-e29b-41d4-a716-446655440001', 5, 1),   -- Tsunami: Hydro Pump
    ('660e8400-e29b-41d4-a716-446655440001', 4, 2),   -- Tsunami: Water Gun
    ('660e8400-e29b-41d4-a716-446655440001', 10, 3),  -- Tsunami: Growl
    ('660e8400-e29b-41d4-a716-446655440001', 1, 4),   -- Tsunami: Tackle
    
    ('660e8400-e29b-41d4-a716-446655440002', 7, 1),   -- Sparky: Thunderbolt
    ('660e8400-e29b-41d4-a716-446655440002', 6, 2),   -- Sparky: Thunder Shock
    ('660e8400-e29b-41d4-a716-446655440002', 13, 3),  -- Sparky: Quick Attack
    ('660e8400-e29b-41d4-a716-446655440002', 1, 4),   -- Sparky: Tackle
    
    ('660e8400-e29b-41d4-a716-446655440003', 11, 1),  -- Draco: Dragon Claw
    ('660e8400-e29b-41d4-a716-446655440003', 12, 2),  -- Draco: Dragon Breath
    ('660e8400-e29b-41d4-a716-446655440003', 3, 3),   -- Draco: Flamethrower
    ('660e8400-e29b-41d4-a716-446655440003', 5, 4),   -- Draco: Hydro Pump
    
    ('660e8400-e29b-41d4-a716-446655440004', 9, 1),   -- Venus: Solar Beam
    ('660e8400-e29b-41d4-a716-446655440004', 8, 2),   -- Venus: Vine Whip
    ('660e8400-e29b-41d4-a716-446655440004', 10, 3),  -- Venus: Growl
    ('660e8400-e29b-41d4-a716-446655440004', 3, 4),   -- Venus: Flamethrower (TM!)
    
    ('660e8400-e29b-41d4-a716-446655440006', 5, 1),   -- Bubbles: Hydro Pump
    ('660e8400-e29b-41d4-a716-446655440006', 4, 2),   -- Bubbles: Water Gun
    ('660e8400-e29b-41d4-a716-446655440006', 10, 3),  -- Bubbles: Growl
    ('660e8400-e29b-41d4-a716-446655440006', 15, 4);  -- Bubbles: Tail Whip

-- ----------------------------------------------------------------------------
-- Sample Type Matchups
-- ----------------------------------------------------------------------------
INSERT INTO type_matchups (attacking_type, defending_type, effectiveness) VALUES
    -- Fire type effectiveness
    ('fire', 'fire', 0.5),
    ('fire', 'water', 0.5),
    ('fire', 'grass', 2.0),
    ('fire', 'ice', 2.0),
    ('fire', 'rock', 0.5),
    ('fire', 'dragon', 0.5),
    ('fire', 'steel', 2.0),
    
    -- Water type effectiveness
    ('water', 'fire', 2.0),
    ('water', 'water', 0.5),
    ('water', 'grass', 0.5),
    ('water', 'ground', 2.0),
    ('water', 'rock', 2.0),
    ('water', 'dragon', 0.5),
    
    -- Electric type effectiveness
    ('electric', 'water', 2.0),
    ('electric', 'electric', 0.5),
    ('electric', 'grass', 0.5),
    ('electric', 'ground', 0),
    ('electric', 'flying', 2.0),
    ('electric', 'dragon', 0.5),
    
    -- Grass type effectiveness
    ('grass', 'fire', 0.5),
    ('grass', 'water', 2.0),
    ('grass', 'grass', 0.5),
    ('grass', 'poison', 0.5),
    ('grass', 'ground', 2.0),
    ('grass', 'flying', 0.5),
    ('grass', 'rock', 2.0),
    ('grass', 'dragon', 0.5),
    ('grass', 'steel', 0.5),
    
    -- Dragon type effectiveness
    ('dragon', 'dragon', 2.0),
    ('dragon', 'steel', 0.5),
    
    -- Normal type effectiveness
    ('normal', 'rock', 0.5),
    ('normal', 'ghost', 0),
    ('normal', 'steel', 0.5);

-- ----------------------------------------------------------------------------
-- Sample Battles
-- ----------------------------------------------------------------------------
INSERT INTO battles (id, status, trainer_1_id, trainer_2_id, winner_id, started_at, ended_at, battle_type, rounds, log) VALUES
    ('770e8400-e29b-41d4-a716-446655440000', 'completed', '550e8400-e29b-41d4-a716-446655440000', '550e8400-e29b-41d4-a716-446655440003', '550e8400-e29b-41d4-a716-446655440000', 
     now() - interval '2 days', now() - interval '2 days' + interval '15 minutes', 'single', 8, 
     '[{"turn": 1, "action": "blaze used flamethrower", "damage": 120}, {"turn": 2, "action": "hydroking used hydro pump", "damage": 100}]'),
    
    ('770e8400-e29b-41d4-a716-446655440001', 'completed', '550e8400-e29b-41d4-a716-446655440001', '550e8400-e29b-41d4-a716-446655440002', '550e8400-e29b-41d4-a716-446655440001',
     now() - interval '1 day', now() - interval '1 day' + interval '20 minutes', 'single', 12,
     '[{"turn": 1, "action": "match started"}]'),
    
    ('770e8400-e29b-41d4-a716-446655440002', 'pending', '550e8400-e29b-41d4-a716-446655440000', '550e8400-e29b-41d4-a716-446655440001', NULL,
     NULL, NULL, 'single', 0, '[]');

-- =============================================================================
-- END OF MIGRATION
-- =============================================================================

