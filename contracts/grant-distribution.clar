
;; title: grant-distribution
;; version: 1.0.0
;; summary: Small Business Grant Distribution System
;; description: A comprehensive system for government and foundation grant distribution
;;              with application processing, eligibility verification, and fund disbursement

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-APPLICATION-NOT-FOUND (err u101))
(define-constant ERR-INVALID-STATUS (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-ALREADY-EXISTS (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))

;; Application Status Constants
(define-constant STATUS-PENDING u0)
(define-constant STATUS-UNDER-REVIEW u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-REJECTED u3)
(define-constant STATUS-FUNDS-DISBURSED u4)

;; Data Variables
(define-data-var next-application-id uint u1)
(define-data-var total-allocated-funds uint u0)
(define-data-var total-disbursed-funds uint u0)

;; Data Maps
(define-map grant-applications
    uint
    {
        applicant: principal,
        business-name: (string-ascii 100),
        requested-amount: uint,
        purpose: (string-ascii 500),
        status: uint,
        submission-time: uint,
        review-notes: (string-ascii 300)
    }
)

(define-map applicant-history
    principal
    {
        applications-count: uint,
        total-received: uint,
        last-application: uint
    }
)

(define-map fund-sources
    principal
    {
        allocated-amount: uint,
        remaining-balance: uint,
        source-name: (string-ascii 100)
    }
)

;; Public Functions

;; Submit a grant application
(define-public (submit-application (business-name (string-ascii 100)) 
                                  (requested-amount uint) 
                                  (purpose (string-ascii 500)))
    (let ((application-id (var-get next-application-id))
          (current-time stacks-block-height))
        (asserts! (> requested-amount u0) ERR-INVALID-AMOUNT)
        
        ;; Create application record
        (map-set grant-applications application-id
            {
                applicant: tx-sender,
                business-name: business-name,
                requested-amount: requested-amount,
                purpose: purpose,
                status: STATUS-PENDING,
                submission-time: current-time,
                review-notes: ""
            }
        )
        
        ;; Update applicant history
        (match (map-get? applicant-history tx-sender)
            existing-history
                (map-set applicant-history tx-sender
                    (merge existing-history
                           { applications-count: (+ (get applications-count existing-history) u1),
                             last-application: application-id }))
            (map-set applicant-history tx-sender
                { applications-count: u1,
                  total-received: u0,
                  last-application: application-id })
        )
        
        ;; Increment next application ID
        (var-set next-application-id (+ application-id u1))
        
        (ok application-id)
    )
)

;; Update application status (admin only)
(define-public (update-application-status (application-id uint) 
                                         (new-status uint) 
                                         (review-notes (string-ascii 300)))
    (let ((application (unwrap! (map-get? grant-applications application-id) ERR-APPLICATION-NOT-FOUND)))
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (<= new-status STATUS-FUNDS-DISBURSED) ERR-INVALID-STATUS)
        
        ;; Update application
        (map-set grant-applications application-id
            (merge application { status: new-status, review-notes: review-notes })
        )
        
        (ok true)
    )
)

;; Add funding source (admin only)
(define-public (add-fund-source (source principal) 
                               (amount uint) 
                               (source-name (string-ascii 100)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        
        ;; Check if source already exists
        (asserts! (is-none (map-get? fund-sources source)) ERR-ALREADY-EXISTS)
        
        ;; Add fund source
        (map-set fund-sources source
            {
                allocated-amount: amount,
                remaining-balance: amount,
                source-name: source-name
            }
        )
        
        ;; Update total allocated funds
        (var-set total-allocated-funds (+ (var-get total-allocated-funds) amount))
        
        (ok true)
    )
)

;; Disburse funds for approved application (admin only)
(define-public (disburse-funds (application-id uint) (funding-source principal))
    (let ((application (unwrap! (map-get? grant-applications application-id) ERR-APPLICATION-NOT-FOUND))
          (fund-source (unwrap! (map-get? fund-sources funding-source) ERR-APPLICATION-NOT-FOUND))
          (requested-amount (get requested-amount application)))
        
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status application) STATUS-APPROVED) ERR-INVALID-STATUS)
        (asserts! (>= (get remaining-balance fund-source) requested-amount) ERR-INSUFFICIENT-FUNDS)
        
        ;; Update application status
        (map-set grant-applications application-id
            (merge application { status: STATUS-FUNDS-DISBURSED })
        )
        
        ;; Update fund source balance
        (map-set fund-sources funding-source
            (merge fund-source { remaining-balance: (- (get remaining-balance fund-source) requested-amount) })
        )
        
        ;; Update applicant history
        (match (map-get? applicant-history (get applicant application))
            existing-history
                (map-set applicant-history (get applicant application)
                    (merge existing-history { total-received: (+ (get total-received existing-history) requested-amount) }))
            false ;; This should not happen if application exists
        )
        
        ;; Update total disbursed funds
        (var-set total-disbursed-funds (+ (var-get total-disbursed-funds) requested-amount))
        
        (ok true)
    )
)

;; Read-only Functions

;; Get application details
(define-read-only (get-application (application-id uint))
    (map-get? grant-applications application-id)
)

;; Get applicant history
(define-read-only (get-applicant-history (applicant principal))
    (map-get? applicant-history applicant)
)

;; Get fund source details
(define-read-only (get-fund-source (source principal))
    (map-get? fund-sources source)
)

;; Get system statistics
(define-read-only (get-system-stats)
    {
        next-application-id: (var-get next-application-id),
        total-allocated-funds: (var-get total-allocated-funds),
        total-disbursed-funds: (var-get total-disbursed-funds),
        remaining-funds: (- (var-get total-allocated-funds) (var-get total-disbursed-funds))
    }
)

;; Check if applicant is eligible (basic check - can be extended)
(define-read-only (check-eligibility (applicant principal) (requested-amount uint))
    (let ((history (map-get? applicant-history applicant)))
        (match history
            existing-data
                ;; Basic eligibility: no more than 3 applications, total received less than 100,000
                (and (< (get applications-count existing-data) u3)
                     (< (get total-received existing-data) u100000)
                     (<= requested-amount u50000))
            ;; First-time applicant
            (<= requested-amount u50000)
        )
    )
)

;; Get applications by status
(define-read-only (get-application-count-by-status (status uint))
    (ok true) ;; Placeholder - would require iteration in full implementation
)

