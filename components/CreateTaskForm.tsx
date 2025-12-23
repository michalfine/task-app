import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Textarea } from "@/components/ui/textarea";
import { Label } from "@/components/ui/label";

interface CreateTaskFormProps {
  onSubmit: (title: string, description: string) => Promise<void>;
}

export function CreateTaskForm({ onSubmit }: CreateTaskFormProps) {
  const [title, setTitle] = useState("");
  const [description, setDescription] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [isSubmitting, setIsSubmitting] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError(null);
    setIsSubmitting(true);
    try {
      await onSubmit(title, description);
      setTitle("");
      setDescription("");
    } catch (err: any) {
      // Provide more helpful error messages
      let errorMessage = "Failed to create task";
      if (err instanceof Error) {
        errorMessage = err.message;
      } else if (typeof err === "string") {
        errorMessage = err;
      } else if (err?.message) {
        errorMessage = err.message;
      }
      
      // Check for common database errors and provide user-friendly messages
      if (errorMessage.includes("profile") || errorMessage.includes("user_id")) {
        errorMessage = "User account setup incomplete. Please sign out and sign in again.";
      } else if (errorMessage.includes("limit")) {
        errorMessage = errorMessage; // Keep the limit message as-is
      } else if (errorMessage.includes("permission") || errorMessage.includes("denied")) {
        errorMessage = "Permission denied. Please contact support if this persists.";
      }
      
      setError(errorMessage);
      console.error("Task creation error:", err);
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-4">
      <div>
        <Label htmlFor="title">Title</Label>
        <Input
          id="title"
          value={title}
          onChange={(e) => setTitle(e.target.value)}
          placeholder="Enter task title"
          required
        />
      </div>
      <div>
        <Label htmlFor="description">Description</Label>
        <Textarea
          id="description"
          value={description}
          onChange={(e) => setDescription(e.target.value)}
          placeholder="Enter task description"
          rows={3}
        />
      </div>
      <Button type="submit" disabled={isSubmitting}>
        {isSubmitting ? "Creating..." : "Create Task"}
      </Button>
      {error && <div className="text-red-500 text-sm mt-2">{error}</div>}
    </form>
  );
}
