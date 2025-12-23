-- Fix task limit trigger to handle missing profile or usage_tracking data gracefully
-- This prevents errors when new users create their first task

CREATE OR REPLACE FUNCTION check_task_limit()
RETURNS trigger
SECURITY DEFINER
SET search_path = public
AS $$
DECLARE
  current_month text;
  monthly_count integer;
  user_limit integer;
BEGIN
  -- Get current month in YYYY-MM format
  current_month := to_char(NOW(), 'YYYY-MM');
  
  -- Get user's task limit from profile (handle case where profile might not exist)
  SELECT tasks_limit INTO user_limit 
  FROM public.profiles 
  WHERE user_id = NEW.user_id;
  
  -- If profile doesn't exist, use default limit
  IF user_limit IS NULL THEN
    user_limit := 100; -- Default limit
  END IF;
  
  -- Get current month's task count (handle case where no usage_tracking row exists)
  SELECT COALESCE(tasks_created, 0) INTO monthly_count
  FROM public.usage_tracking
  WHERE user_id = NEW.user_id 
  AND year_month = current_month;

  -- If no usage_tracking row exists, monthly_count will be NULL, set to 0
  IF monthly_count IS NULL THEN
    monthly_count := 0;
  END IF;

  -- Check if limit would be exceeded
  IF monthly_count >= user_limit THEN
      RAISE EXCEPTION 'Monthly task limit of % reached', user_limit;
  END IF;
  
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

