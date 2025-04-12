(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-APPLIED (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-NOT-ELIGIBLE (err u105))

(define-data-var fund-balance uint u0)
(define-data-var next-application-id uint u1)
(define-data-var admin principal tx-sender)

(define-map Applications
    uint 
    {
        applicant: principal,
        gpa: uint,
        major: (string-ascii 64),
        amount-requested: uint,
        status: (string-ascii 20)
    }
)

(define-map ApplicantStatus
    principal 
    {
        has-active-application: bool,
        received-funds: uint
    }
)

(define-public (initialize-contract)
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (ok true)))

(define-public (donate-to-fund)
    (let
        ((amount (stx-get-balance tx-sender)))
        (begin
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            (var-set fund-balance (+ (var-get fund-balance) amount))
            (ok amount))))

(define-public (apply-for-scholarship (gpa uint) (major (string-ascii 64)) (amount uint))
    (let
        ((applicant-status (default-to 
            { has-active-application: false, received-funds: u0 }
            (map-get? ApplicantStatus tx-sender)))
         (application-id (var-get next-application-id)))
        (begin
            (asserts! (not (get has-active-application applicant-status)) ERR-ALREADY-APPLIED)
            (asserts! (>= gpa u300) ERR-NOT-ELIGIBLE)
            (asserts! (<= amount (var-get fund-balance)) ERR-INSUFFICIENT-FUNDS)
            (map-set Applications application-id
                {
                    applicant: tx-sender,
                    gpa: gpa,
                    major: major,
                    amount-requested: amount,
                    status: "pending"
                })
            (map-set ApplicantStatus tx-sender
                {
                    has-active-application: true,
                    received-funds: u0
                })
            (var-set next-application-id (+ application-id u1))
            (ok application-id))))

(define-public (approve-application (application-id uint))
    (let
        ((application (unwrap! (map-get? Applications application-id) ERR-NOT-FOUND)))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status application) "pending") ERR-NOT-ELIGIBLE)
            (try! (as-contract (stx-transfer? (get amount-requested application) tx-sender (get applicant application))))
            (var-set fund-balance (- (var-get fund-balance) (get amount-requested application)))
            (map-set Applications application-id
                (merge application { status: "approved" }))
            (map-set ApplicantStatus (get applicant application)
                {
                    has-active-application: false,
                    received-funds: (get amount-requested application)
                })
            (ok true))))

(define-public (reject-application (application-id uint))
    (let
        ((application (unwrap! (map-get? Applications application-id) ERR-NOT-FOUND)))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status application) "pending") ERR-NOT-ELIGIBLE)
            (map-set Applications application-id
                (merge application { status: "rejected" }))
            (map-set ApplicantStatus (get applicant application)
                {
                    has-active-application: false,
                    received-funds: u0
                })
            (ok true))))

(define-read-only (get-application (application-id uint))
    (ok (unwrap! (map-get? Applications application-id) ERR-NOT-FOUND)))

(define-read-only (get-fund-balance)
    (ok (var-get fund-balance)))

(define-read-only (get-applicant-status (applicant principal))
    (ok (default-to 
        { has-active-application: false, received-funds: u0 }
        (map-get? ApplicantStatus applicant))))
