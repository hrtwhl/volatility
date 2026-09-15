/**
 * A button-group input, compatible with Framework's `view()`.
 *
 * Observable's built-in Inputs.radio renders markup that is awkward to restyle
 * into a segmented control, so this returns a plain element that sets `.value`
 * and fires "input" — the only contract `view()` needs.
 */
export function buttonGroup(options, { value = options[0].value, label } = {}) {
  const group = document.createElement("div");
  group.className = "range-control";
  group.setAttribute("role", "group");
  if (label) group.setAttribute("aria-label", label);

  let current = value;

  const buttons = options.map((option) => {
    const button = document.createElement("button");
    button.type = "button";
    button.textContent = option.label;
    button.setAttribute("aria-pressed", String(option.value === current));

    button.addEventListener("click", () => {
      current = option.value;
      buttons.forEach((other, i) =>
        other.setAttribute("aria-pressed", String(options[i].value === current))
      );
      group.value = current;
      group.dispatchEvent(new CustomEvent("input", { bubbles: true }));
    });

    group.append(button);
    return button;
  });

  group.value = current;
  return group;
}
