# Debugging Task Creation Error

If you're still getting database errors when creating tasks, follow these steps:

## Step 1: Check the Exact Error Message

Open your browser's Developer Console (F12) and look for the exact error message. Common errors:

- `foreign key constraint` - Profile doesn't exist
- `permission denied` - RLS policy blocking
- `limit reached` - Task limit exceeded
- `PGRST116` - Row not found

## Step 2: Verify Database Setup

Run these SQL queries in your Supabase SQL Editor to check:

```sql
-- Check if your profile exists
SELECT * FROM public.profiles WHERE user_id = auth.uid();

-- Check if you can insert into profiles
INSERT INTO public.profiles (user_id, name) 
VALUES (auth.uid(), 'Test') 
ON CONFLICT (user_id) DO NOTHING;

-- Check RLS policies
SELECT * FROM pg_policies WHERE tablename = 'profiles';
SELECT * FROM pg_policies WHERE tablename = 'tasks';
```

## Step 3: Apply the Migration

Make sure migration `8_fix_profile_insert_policy.sql` has been applied:

```sql
-- Run this in Supabase SQL Editor
CREATE POLICY IF NOT EXISTS "Users can insert own profile"
ON public.profiles FOR INSERT
WITH CHECK (auth.uid() = user_id);

CREATE POLICY IF NOT EXISTS "Users can update own profile"
ON public.profiles FOR UPDATE
USING (auth.uid() = user_id)
WITH CHECK (auth.uid() = user_id);
```

## Step 4: Check Edge Function Logs

In Supabase Dashboard:
1. Go to Edge Functions
2. Click on `create-task-with-ai`
3. Check the Logs tab for detailed error messages

## Step 5: Manual Profile Creation

If profile is missing, create it manually:

```sql
-- Replace YOUR_USER_ID with your actual user ID
INSERT INTO public.profiles (user_id, name, subscription_plan, tasks_limit)
VALUES (
  'YOUR_USER_ID',
  'Your Name',
  'free',
  100
)
ON CONFLICT (user_id) DO NOTHING;
```

## Common Issues and Fixes

### Issue: "foreign key constraint" error
**Fix**: Profile doesn't exist. The edge function should create it, but if it fails, manually create it using Step 5.

### Issue: "permission denied" error
**Fix**: RLS policies not set correctly. Apply migration from Step 3.

### Issue: "limit reached" error
**Fix**: You've hit your monthly task limit. Check your usage:
```sql
SELECT * FROM public.usage_tracking WHERE user_id = auth.uid();
```

### Issue: Edge function timeout
**Fix**: OpenAI API might be slow. Check edge function logs for timeout errors.

## Still Having Issues?

1. Check browser console for the exact error
2. Check Supabase Edge Function logs
3. Verify all migrations have been applied
4. Try creating a profile manually
5. Check if RLS is enabled and policies exist

