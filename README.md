# Blockchain-Based Scholarship Fund Smart Contract

A transparent and automated scholarship distribution system built on Stacks blockchain using Clarity smart contracts.

## Features

- Scholarship fund management
- Application submission
- Automated disbursement
- Transparent selection process
- Real-time fund balance tracking

## Contract Functions

### For Donors
- `donate-to-fund`: Contribute STX to the scholarship fund

### For Students
- `apply-for-scholarship`: Submit scholarship application with GPA, major, and requested amount
- `get-applicant-status`: Check application status

### For Administrators
- `approve-application`: Approve and disburse scholarship funds
- `reject-application`: Reject scholarship applications
- `initialize-contract`: Set up initial contract parameters

### Public Views
- `get-application`: View specific application details
- `get-fund-balance`: Check current fund balance

## Requirements

- Minimum GPA: 3.00 (represented as 300)
- Valid major field
- Requested amount must not exceed fund balance

## Usage

1. Deploy contract using Clarinet
2. Initialize contract with administrator
3. Accept donations to fund
4. Process student applications
5. Approve/reject applications
6. Automatic fund disbursement on approval

## Security

- Only administrator can approve/reject applications
- Students cannot submit multiple active applications
- Automatic balance checks before disbursement