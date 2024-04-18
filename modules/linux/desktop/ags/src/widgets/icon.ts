import Widget from "resource:///com/github/Aylur/ags/widget.js";

export const MaterialIcon = (icon, className = "", props = {}) =>
  Widget.Label({
    className: `icon icon-material ${className}`,
    label: icon,
    ...props,
  });
