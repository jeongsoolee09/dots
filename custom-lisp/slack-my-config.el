(require 'slack)

(slack-register-team
 :name "Umasupporter"
 :token (auth-source-pick-first-password
         :machine "umasupporter.slack.com"
         :user "umauma")
 :cookie (auth-source-pick-first-password
          :machine "umasupporter.slack.com"
          :user "umauma^cookie"))
(slack-register-team
 :name "Clojurians"
 :token (auth-source-pick-first-password
         :machine "clojurians.slack.com"
         :user "clojure")
 :cookie (auth-source-pick-first-password
          :machine "clojurians.slack.com"
          :user "clojure^cookie"))

(provide 'slack-my-config)
