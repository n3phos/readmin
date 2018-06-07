module FormatDate exposing (formatDate, toString, toVoucherFormat, formatVoucherDate)

import Date exposing (Date)
import Date.Extra.Format as DF
import Date.Extra.Config.Config_de_de exposing (config)


formatDate : Date -> String
formatDate =
    DF.format config "%d.%m.%Y"


toString : Date -> String
toString =
    DF.format config "%Y-%m-%d"


toVoucherFormat : Date -> String
toVoucherFormat =
    DF.format config "%Y-%d-%m_%H-%M-%S"


formatVoucherDate : Date -> String
formatVoucherDate =
    DF.format config "%Y-%m-%d %H:%M"
