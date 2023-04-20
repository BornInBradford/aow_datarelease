# aow_datarelease/denom

Tools and scripts for producing Age of Wonder data release denominator data files.

Takes information from Consent and School_RecruitmentList tables.

Favours information from School_RecruitmentList in case of conflicts.

Three outputs are generated:

* denom_identifiable - complete denominator information, including some school identifying info such as school name and establishment number; pupil personal information is dropped, apart from upn and postcode
* denom_pseudo - only includes pseudo versions of upn and the school identifying info, e.g. as aow_person_id, school_id; also postcode is dropped
* id_lookup - a lookup table linking aow_recruitment_id and aow_person_id

Consent information is currently included in full but maybe needs to be separated in future. Some recoding will be needed as all fields are present as binaries yet not all are options under all consent scenarios.
