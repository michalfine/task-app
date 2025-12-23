// Setup type definitions for built-in Supabase Runtime APIs
import "jsr:@supabase/functions-js/edge-runtime.d.ts";
import { createClient } from "jsr:@supabase/supabase-js@2";
import OpenAI from "npm:openai";

// Load environment variables
const SUPABASE_URL = Deno.env.get("SUPABASE_URL") ?? "";
const SUPABASE_ANON_KEY = Deno.env.get("SUPABASE_ANON_KEY") ?? "";
const OPENAI_API_KEY = Deno.env.get("OPENAI_API_KEY");

const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "POST",
  "Access-Control-Allow-Headers":
    "authorization, x-client-info, apikey, content-type",
};

Deno.serve(async (req) => {
  if (req.method === "OPTIONS") {
    return new Response(null, { status: 204, headers: corsHeaders });
  }

  try {
    const { title, description } = await req.json();

    console.log("🔄 Creating task with AI suggestions...");
    const authHeader = req.headers.get("Authorization");
    if (!authHeader) {
      throw new Error("No authorization header");
    }

    // Initialize Supabase client
    const supabaseClient = createClient(SUPABASE_URL, SUPABASE_ANON_KEY, {
      global: {
        headers: { Authorization: authHeader },
      },
    });

    // Get user session
    const {
      data: { user },
    } = await supabaseClient.auth.getUser();
    if (!user) throw new Error("No user found");

    // Ensure profile exists (in case trigger didn't run)
    const { data: profile, error: profileError } = await supabaseClient
      .from("profiles")
      .select("user_id")
      .eq("user_id", user.id)
      .maybeSingle();

    if (profileError) {
      console.error("Error checking profile:", profileError);
      throw new Error("Database error checking profile. Please try again.");
    }

    if (!profile) {
      // Profile doesn't exist, create it
      console.log("Profile not found, creating profile for user:", user.id);
      const { error: createProfileError } = await supabaseClient
        .from("profiles")
        .insert({
          user_id: user.id,
          name: user.user_metadata?.name || user.email?.split("@")[0] || "User",
        })
        .select()
        .single();

      if (createProfileError) {
        console.error("Error creating profile:", createProfileError);
        // Check if it's a permission error
        if (createProfileError.code === "42501" || createProfileError.message?.includes("permission")) {
          throw new Error("Permission denied. Please contact support.");
        }
        throw new Error(`Failed to create user profile: ${createProfileError.message}`);
      }
      console.log("Profile created successfully");
    }

    // Create the task
    const { data, error } = await supabaseClient
      .from("tasks")
      .insert({
        title,
        description,
        completed: false,
        user_id: user.id,
      })
      .select()
      .single();

    if (error) {
      console.error("Error creating task:", error);
      console.error("Error details:", JSON.stringify(error, null, 2));
      
      // Provide more helpful error messages
      if (error.code === "23503") {
        throw new Error("User profile not found. Please sign out and sign in again.");
      } else if (error.code === "P0001" || error.message?.includes("limit")) {
        // P0001 is a custom exception (RAISE EXCEPTION)
        throw new Error(error.message || "Task limit reached");
      } else if (error.code === "42501") {
        throw new Error("Permission denied. Please contact support.");
      } else if (error.message?.includes("foreign key")) {
        throw new Error("Database configuration error. Please contact support.");
      }
      
      // Return the original error message for debugging
      throw new Error(`Failed to create task: ${error.message || JSON.stringify(error)}`);
    }

    // Initialize OpenAI
    const openai = new OpenAI({
      apiKey: OPENAI_API_KEY,
    });

    // Get label suggestion from OpenAI
    const prompt = `Based on this task title: "${title}" and description: "${description}", suggest ONE of these labels: work, personal, priority, shopping, home. Reply with just the label word and nothing else.`;

    const completion = await openai.chat.completions.create({
      messages: [{ role: "user", content: prompt }],
      model: "gpt-4o-mini",
      temperature: 0.3,
      max_tokens: 16,
    });

    const suggestedLabel = completion.choices[0].message.content
      ?.toLowerCase()
      .trim();

    console.log(`✨ AI Suggested Label: ${suggestedLabel}`);

    // Validate the label
    const validLabels = ["work", "personal", "priority", "shopping", "home"];
    const label = validLabels.includes(suggestedLabel) ? suggestedLabel : null;

    // Update the task with the suggested label
    const { data: updatedTask, error: updateError } = await supabaseClient
      .from("tasks")
      .update({ label })
      .eq("task_id", data.task_id)
      .select()
      .single();

    if (updateError) throw updateError;

    return new Response(JSON.stringify(updatedTask), {
      headers: {
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
      },
    });
  } catch (error: any) {
    const errorMessage = error?.message || error?.toString() || "Unknown error";
    console.error("Error in create-task-with-ai:", errorMessage);
    console.error("Full error:", error);
    
    return new Response(
      JSON.stringify({ 
        error: errorMessage,
        code: error?.code,
        details: process.env.NODE_ENV === "development" ? error : undefined
      }), 
      {
        status: 400,
        headers: { ...corsHeaders, "Content-Type": "application/json" },
      }
    );
  }
});
