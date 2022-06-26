(setq twitch-api-username "namudontdie")

(setq twitch-api-oauth-token
      (auth-source-pick-first-password
       :machine "twitch.tv"
       :user "namudontdie"))

(provide 'twitch-my-config)
