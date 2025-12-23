import { useAuth } from "@/hooks/useAuth";
import { usePathname, useRouter } from "next/navigation";
import { useEffect, useState } from "react";
import { LoadingSkeleton } from "./LoadingSkeleton";

const PUBLIC_ROUTES = ["/"];
// Auth callback routes that should not be blocked
const AUTH_CALLBACK_ROUTES = ["/auth/callback"];
const DEFAULT_AUTHENTICATED_ROUTE = "/dashboard";
const DEFAULT_PUBLIC_ROUTE = "/";

export function RouteGuard({ children }: { children: React.ReactNode }) {
  const { isLoggedIn, isLoading } = useAuth();
  const pathname = usePathname();
  const router = useRouter();
  const [isReady, setIsReady] = useState(false);

  useEffect(() => {
    // Always wait for auth to finish loading before making routing decisions
    if (isLoading) {
      setIsReady(false);
      return;
    }

    const isPublicRoute = PUBLIC_ROUTES.includes(pathname);
    const isAuthCallbackRoute = AUTH_CALLBACK_ROUTES.includes(pathname);

    // Allow auth callback routes to proceed (they handle their own redirects)
    if (isAuthCallbackRoute) {
      setIsReady(true);
      return;
    }

    // If authenticated and on public route, redirect to dashboard
    if (isLoggedIn && pathname === DEFAULT_PUBLIC_ROUTE) {
      router.replace(DEFAULT_AUTHENTICATED_ROUTE);
      setIsReady(false);
      return;
    }

    // If not authenticated and not on public route, redirect to login
    if (!isLoggedIn && !isPublicRoute) {
      router.replace(DEFAULT_PUBLIC_ROUTE);
      setIsReady(false);
      return;
    }

    // All checks passed, allow route to render
    setIsReady(true);
  }, [isLoggedIn, isLoading, pathname, router]);

  // Show loading while auth is initializing or route is being determined
  if (isLoading || !isReady) {
    return <LoadingSkeleton />;
  }

  return <>{children}</>;
}
