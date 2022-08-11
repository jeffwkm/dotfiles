{ config, lib }: {
  cfg = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
          <key>MachServices</key>
    	    <dict>
              <key>com.custom.pre-login-daemon</key>
              <true/>
    	    </dict>
    	    <key>Label</key>
    	    <string>com.custom.pre-login</string>
          <key>UserName</key>
          <string>jeff</string>
    	    <key>Program</key>
    	    <string>/Users/jeff/bin/pre-login</string>
          <key>WorkingDirectory</key>
          <string>/Users/jeff</string>
    	    <key>RunAtLoad</key>
    	    <true/>
          <key>StandardOutPath</key>
          <string>/Users/jeff/pre-login.out</string>
          <key>StandardErrorPath</key>
          <string>/Users/jeff/pre-login.err</string>
      </dict>
      </plist>
  '';
}
