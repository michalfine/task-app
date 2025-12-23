-- Add INSERT policy for profiles so users can create their own profile if trigger fails
-- This is a safety net in case the automatic profile creation trigger doesn't run

-- Security policy: Users can insert their own profile
create policy "Users can insert own profile"
on public.profiles for insert
with check (auth.uid() = user_id);

-- Security policy: Users can update their own profile
create policy "Users can update own profile"
on public.profiles for update
using (auth.uid() = user_id)
with check (auth.uid() = user_id);

