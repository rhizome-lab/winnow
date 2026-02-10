export const ui = {
  show_message(text: string): void {
    alert(text);
  },

  show_choices(prompt: string, choices: string[]): number {
    const message = choices
      .map((c, i) => `${i + 1}. ${c}`)
      .join("\n");
    const result = window.prompt(`${prompt}\n\n${message}`, "1");
    const index = parseInt(result || "1", 10) - 1;
    return Math.max(0, Math.min(index, choices.length - 1));
  },

  show_text_input(prompt: string, defaultValue: string): string {
    return window.prompt(prompt, defaultValue) || defaultValue;
  },

  show_number_input(
    prompt: string,
    defaultValue: number,
    min: number,
    max: number,
  ): number {
    const result = window.prompt(prompt, String(defaultValue));
    const num = parseFloat(result || String(defaultValue));
    return Math.max(min, Math.min(num, max));
  },

  update(): void {
    // No-op for now; future: process overlay animations.
  },
};
