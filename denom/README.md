# aow_datarelease/denom
Tools and scripts for producing Age of Wonder data release denominator data files.

Takes information from Consent and School_RecruitmentList tables.

Favours information from School_RecruitmentList in case of conflicts.

Pupil personal information is dropped, apart from UPN and postcode as we might need these yet. But school identifying information such as establishment number and form tutor is currently kept. 

Pseudo'd versions of the school identifiers will need to be generated for publishing, and a person ID in place of UPN.

Consent information is currently included in full but maybe needs to be separated in future. Some recoding will be needed as all fields are present as binaries yet not all are options under all consent scenarios.

