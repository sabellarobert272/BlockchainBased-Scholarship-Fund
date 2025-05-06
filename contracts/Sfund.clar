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



(define-data-var total-applications uint u0)
(define-data-var total-approved uint u0) 
(define-data-var total-rejected uint u0)
(define-data-var total-funds-distributed uint u0)

(define-read-only (get-fund-statistics)
    (ok {
        total-applications: (var-get total-applications),
        total-approved: (var-get total-approved),
        total-rejected: (var-get total-rejected),
        total-funds-distributed: (var-get total-funds-distributed),
        current-balance: (var-get fund-balance)
    }))

;; Update the apply-for-scholarship function to add:
(var-set total-applications (+ (var-get total-applications) u1))

;; Update the approve-application function to add:
(var-set total-approved (+ (var-get total-approved) u1))
(var-set total-funds-distributed (+ (var-get total-funds-distributed) ))

;; Update the reject-application function to add:
(var-set total-rejected (+ (var-get total-rejected) u1))

(define-public (withdraw-application (application-id uint))
    (let (
        (application (unwrap! (map-get? Applications application-id) ERR-NOT-FOUND))
        )
        (begin
            (asserts! (is-eq tx-sender (get applicant application)) ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status application) "pending") ERR-NOT-ELIGIBLE)
            (map-set Applications application-id
                (merge application { status: "withdrawn" }))
            (map-set ApplicantStatus tx-sender
                {
                    has-active-application: false,
                    received-funds: u0
                })
            (ok true))))


(define-constant MILESTONE-1 u1000000) 
(define-constant MILESTONE-2 u5000000)
(define-constant MILESTONE-3 u10000000)
(define-constant BONUS-AMOUNT u1000)

(define-map MilestoneReached
    uint
    bool
)

(define-public (claim-milestone-bonus)
    (let (
        (current-balance (var-get fund-balance))
        (milestone-1-claimed (default-to false (map-get? MilestoneReached u1)))
        (milestone-2-claimed (default-to false (map-get? MilestoneReached u2)))
        (milestone-3-claimed (default-to false (map-get? MilestoneReached u3)))
        )
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (if (and (>= current-balance MILESTONE-1) (not milestone-1-claimed))
                (begin
                    (try! (as-contract (stx-transfer? BONUS-AMOUNT tx-sender (var-get admin))))
                    (map-set MilestoneReached u1 true)
                    (ok true)
                )
                (if (and (>= current-balance MILESTONE-2) (not milestone-2-claimed))
                    (begin
                        (try! (as-contract (stx-transfer? BONUS-AMOUNT tx-sender (var-get admin))))
                        (map-set MilestoneReached u2 true)
                        (ok true)
                    )
                    (if (and (>= current-balance MILESTONE-3) (not milestone-3-claimed))
                        (begin
                            (try! (as-contract (stx-transfer? BONUS-AMOUNT tx-sender (var-get admin))))
                            (map-set MilestoneReached u3 true)
                            (ok true)
                        )
                        ERR-NOT-ELIGIBLE
                    )
                )
            )
        )
    ))

(define-read-only (get-milestone-status)
    (ok {
        milestone-1: (default-to false (map-get? MilestoneReached u1)),
        milestone-2: (default-to false (map-get? MilestoneReached u2)),
        milestone-3: (default-to false (map-get? MilestoneReached u3))
    }))