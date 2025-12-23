"use client";

import { useEffect, useState } from "react";
import { useRouter, useSearchParams } from "next/navigation";
import { createBrowserClient } from "@supabase/ssr";

export default function AuthCallback() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const [status, setStatus] = useState<"loading" | "success" | "error">("loading");
  const [errorMessage, setErrorMessage] = useState<string | null>(null);

  useEffect(() => {
    const handleCallback = async () => {
      const supabase = createBrowserClient(
        process.env.NEXT_PUBLIC_SUPABASE_URL!,
        process.env.NEXT_PUBLIC_SUPABASE_ANON_KEY!
      );

      try {
        // Get the code from URL (for OAuth) or hash (for magic link)
        const code = searchParams.get("code");
        const error = searchParams.get("error");
        const errorDescription = searchParams.get("error_description");
        const type = searchParams.get("type"); // For magic links: "recovery" or "signup"

        // Handle OAuth errors
        if (error) {
          console.error("OAuth error:", error, errorDescription);
          setStatus("error");
          setErrorMessage(errorDescription || error);
          setTimeout(() => {
            router.replace(`/?error=${encodeURIComponent(errorDescription || error)}`);
          }, 2000);
          return;
        }

        // Exchange code for session (for OAuth callbacks)
        if (code) {
          const { data, error: exchangeError } = await supabase.auth.exchangeCodeForSession(code);
          
          if (exchangeError) {
            console.error("Error exchanging code for session:", exchangeError);
            setStatus("error");
            setErrorMessage(exchangeError.message);
            setTimeout(() => {
              router.replace(`/?error=${encodeURIComponent(exchangeError.message)}`);
            }, 2000);
            return;
          }

          if (data.session) {
            setStatus("success");
            // Small delay to ensure session is fully set
            setTimeout(() => {
              router.replace("/dashboard");
            }, 500);
            return;
          }
        }

        // For magic links, check if we have a hash fragment
        // Supabase SSR should have already processed it, but let's verify
        if (type === "recovery" || type === "signup") {
          // Wait a bit for Supabase to process the hash
          await new Promise((resolve) => setTimeout(resolve, 1000));
        }

        // Check if we have a session (for magic links or if code exchange already happened)
        const {
          data: { session },
          error: sessionError,
        } = await supabase.auth.getSession();

        if (sessionError) {
          console.error("Error getting session:", sessionError);
          setStatus("error");
          setErrorMessage(sessionError.message);
          setTimeout(() => {
            router.replace(`/?error=${encodeURIComponent(sessionError.message)}`);
          }, 2000);
          return;
        }

        if (session) {
          setStatus("success");
          setTimeout(() => {
            router.replace("/dashboard");
          }, 500);
        } else {
          setStatus("error");
          setErrorMessage("No session found. Please try signing in again.");
          setTimeout(() => {
            router.replace("/?error=Authentication failed");
          }, 2000);
        }
      } catch (error: any) {
        console.error("Error in auth callback:", error);
        setStatus("error");
        setErrorMessage(error.message || "Authentication failed");
        setTimeout(() => {
          router.replace(`/?error=${encodeURIComponent(error.message || "Authentication failed")}`);
        }, 2000);
      }
    };

    handleCallback();
  }, [router, searchParams]);

  return (
    <div className="flex items-center justify-center min-h-screen">
      <div className="text-center">
        {status === "loading" && (
          <>
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-white mx-auto"></div>
            <p className="mt-4 text-white">Completing authentication...</p>
          </>
        )}
        {status === "success" && (
          <>
            <div className="text-green-400 text-4xl mb-4">✓</div>
            <p className="text-white">Authentication successful! Redirecting...</p>
          </>
        )}
        {status === "error" && (
          <>
            <div className="text-red-400 text-4xl mb-4">✗</div>
            <p className="text-white">Authentication failed</p>
            {errorMessage && (
              <p className="text-red-300 text-sm mt-2">{errorMessage}</p>
            )}
            <p className="text-white text-sm mt-4">Redirecting to login...</p>
          </>
        )}
      </div>
    </div>
  );
}

