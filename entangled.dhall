let entangled = https://raw.githubusercontent.com/entangled/entangled/v1.2.2/data/config-schema.dhall
                sha256:9bb4c5649869175ad0b662d292fd81a3d5d9ccb503b1c7e316d531b7856fb096

let languages = entangled.languages #
    [ { name = "Sed", identifiers = ["sed"], comment = entangled.comments.hash } ]

in { entangled = entangled.Config :: { watchList = [ "lit/*.md" ] : List Text
                                     , languages = languages
                                     }
   }

