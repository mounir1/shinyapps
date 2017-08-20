#### FireData
[![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/Kohze/FireData/master/LICENSE.txt)
[![Build Status](https://travis-ci.org/Kohze/FireData.svg?branch=master)](https://travis-ci.org/Kohze/FireData)
[![codecov](https://codecov.io/gh/Kohze/FireData/branch/master/graph/badge.svg)](https://codecov.io/gh/Kohze/FireData)


**Project Title:** FireData: Connecting R to Google Firebase

--- 

Mileston 2 (24-07-2017):

- [x] integrated fireData in the spatialMaps project (currently fork is pending)
- [x] Added more secure logins by adding prompts
- [x] Extending the descriptions/documentation
- [x] added unit tests and reached 100% test coverage 

Milestone 1 (25-06-2017):

- [x] user authentication
- [x] general API functionality (upload, download)
- [x] user registration/pw resett
- [x] token generation

---

FireData integrates R Datascience with the abilities and possibilities of the Google Firebase Framework.

Firebase makes it possible to exchange in real-time information in the Cloud and thereby opening the R language to IoT hardware/data processing. Other than that it is also immensely useful for shiny application to integrate Social Authentications and persistent data & uploads.

---


### Getting Started

- visit at https://firebase.google.com/
- login and enter the "console"
- add new project
- find the API keys at the "overview" page clicking on add firebase to web-app

### Examples

- uploading a data.frame:

```upload(x = mtcars, projectURL = "https://firedata-b0e54.firebaseio.com/",  directory = "main")```

- download a data.frame:

```download(projectURL = "https://firedata-b0e54.firebaseio.com/directory", fileName = "-KqxJtP4-cwkpnvLgAtw")```

- register a new user

```createUser(projectAPI = "AIzaSyAjZLO9-CRV3gObpwdFz-k8AiTOxHSBmdc", email = "your@email.com", password = "12341234" ))```


more examples will be added soon



<script src="https://www.gstatic.com/firebasejs/4.2.0/firebase.js"></script>
<script>
  // Initialize Firebase
  var config = {
    apiKey: "AIzaSyAGRwfwlSKwkS9S62EXwl1gP6vw_R_y5fU",
    authDomain: "shiny-6ed23.firebaseapp.com",
    databaseURL: "https://shiny-6ed23.firebaseio.com",
    projectId: "shiny-6ed23",
    storageBucket: "shiny-6ed23.appspot.com",
    messagingSenderId: "695589367614"
  };
  firebase.initializeApp(config);
</script>