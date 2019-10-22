(in-package :lib)

(defproto copper-coin (currency)
  (brief "a copper coin")
  (icon 'bronze-coin)
  (full "Copper coins are the most common currency of the realm, used by
    purchase most typical, everyday items."))

(defproto silver-coin (currency)
  (brief "a silver coin")
  (icon 'silver-coin)
  (full "Silver coins are uncommon, especially among poorer folk. They are used
    to purchase more expensive goods."))

(defproto gold-coin (currency)
  (brief "a gold coin")
  (icon 'gold-coin)
  (full "Most people never see a gold coin. They are used by the truly wealthy
    to purchase luxury items, or by merchants dealing in large volumes of trade
    goods."))
