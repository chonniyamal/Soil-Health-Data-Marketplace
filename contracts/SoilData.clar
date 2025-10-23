;; title: SoilData
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;
(define-constant err-unauthorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-price (err u103))
(define-constant err-insufficient-payment (err u104))
(define-constant err-already-purchased (err u105))

;; data vars
;;
(define-data-var next-data-id uint u1)

;; data maps
;;
(define-map soil-data-registry
  { data-id: uint }
  {
    owner: principal,
    location: (string-ascii 100),
    ph-level: uint,
    nitrogen-content: uint,
    phosphorus-content: uint,
    potassium-content: uint,
    organic-matter: uint,
    price: uint,
    timestamp: uint
  }
)

(define-map data-access
  { data-id: uint, buyer: principal }
  { purchased: bool, purchase-timestamp: uint }
)

;; public functions
;;
(define-public (register-soil-data
  (location (string-ascii 100))
  (ph-level uint)
  (nitrogen-content uint)
  (phosphorus-content uint)
  (potassium-content uint)
  (organic-matter uint)
  (price uint)
)
  (let
    (
      (data-id (var-get next-data-id))
      (caller tx-sender)
    )
    (asserts! (> price u0) err-invalid-price)
    (map-set soil-data-registry
      { data-id: data-id }
      {
        owner: caller,
        location: location,
        ph-level: ph-level,
        nitrogen-content: nitrogen-content,
        phosphorus-content: phosphorus-content,
        potassium-content: potassium-content,
        organic-matter: organic-matter,
        price: price,
        timestamp: stacks-block-height
      }
    )
    (var-set next-data-id (+ data-id u1))
    (ok data-id)
  )
)

(define-public (purchase-data-access (data-id uint))
  (let
    (
      (data (unwrap! (map-get? soil-data-registry { data-id: data-id }) err-not-found))
      (buyer tx-sender)
      (owner (get owner data))
      (price (get price data))
    )
    (asserts! (not (is-eq buyer owner)) err-unauthorized)
    (asserts! (is-none (map-get? data-access { data-id: data-id, buyer: buyer })) err-already-purchased)
    (try! (stx-transfer? price buyer owner))
    (map-set data-access
      { data-id: data-id, buyer: buyer }
      { purchased: true, purchase-timestamp: stacks-block-height }
    )
    (ok true)
  )
)

(define-public (update-data-price (data-id uint) (new-price uint))
  (let
    (
      (data (unwrap! (map-get? soil-data-registry { data-id: data-id }) err-not-found))
      (caller tx-sender)
    )
    (asserts! (is-eq caller (get owner data)) err-unauthorized)
    (asserts! (> new-price u0) err-invalid-price)
    (map-set soil-data-registry
      { data-id: data-id }
      (merge data { price: new-price })
    )
    (ok true)
  )
)

;; read only functions
;;
(define-read-only (get-soil-data (data-id uint))
  (ok (map-get? soil-data-registry { data-id: data-id }))
)

(define-read-only (has-access (data-id uint) (user principal))
  (let
    (
      (data (unwrap! (map-get? soil-data-registry { data-id: data-id }) err-not-found))
      (owner (get owner data))
    )
    (if (is-eq user owner)
      (ok true)
      (ok (is-some (map-get? data-access { data-id: data-id, buyer: user })))
    )
  )
)

(define-read-only (get-data-price (data-id uint))
  (let
    (
      (data (unwrap! (map-get? soil-data-registry { data-id: data-id }) err-not-found))
    )
    (ok (get price data))
  )
)

(define-read-only (get-next-data-id)
  (ok (var-get next-data-id))
)

;; private functions
;;

