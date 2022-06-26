(require 'erc)

(setq erc-enable-sasl-auth t
      erc-server-list
      '(("irc.libera.chat"
         :port "6697"
         :ssl t
         :nick "FlyingBanana"
         :password (auth-source-pick-first-password
                    :machine "irc.libera.chat"
                    :user "FlyingBanana"
                    :port 6697))))

(provide 'erc-my-config)
