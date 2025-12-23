-- Enable pgcrypto extension (required by Supabase Auth + gen_random_uuid)
create extension if not exists "pgcrypto";
