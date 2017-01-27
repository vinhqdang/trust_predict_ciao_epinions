# Item-rating prediction in social-based recommedation networks

In this study, we aim to predict the rating scores of a user to an item, given two training datasets.

- Rating dataset.
- Trust dataset.

## Datasets

### Rating dataset

Given as a set of tuple:

``(u,i,t,r)``


wherein:

- u: user ID who gives the rate to the item
- i: item ID
- t: timestamp when the rate is made
- r: rating score, from 1 to 5

### Trust dataset

Given as a set of tuple:

``(u1,u2,t,v)``

wherein:

- u1: trustor ID
- u2: trustee ID
- u3: timestamp when the relation is declared
- v: value (trust/distrust)

## Implementation

You should install [R >= 3.3.2](r-project.org)

``
source ("Process.R")
``

Two functions are provided:

- ``rating_prediction``: predict without trust information.
- ``rate_trust_prediction``: predict with trust information.

