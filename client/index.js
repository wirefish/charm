String.prototype.strip = function()
{
    return this.replace(/^\s+|\s+$/g, '');
}

function setPrompt(id, msg, warning)
{
    prompt = document.getElementById(id);
    prompt.innerHTML = msg;
    prompt.setAttribute("class", warning ? "warning" : "prompt");
}

function showLogin()
{
    setPrompt("lprompt", "Already signed up? Enter your username and password.");
    setPrompt("cprompt", "To get started, choose your username and password.");

    var fields = ['cuser', 'cpass1', 'cpass2', 'luser', 'lpass'];
    fields.map(function(f) { document.getElementById(f).value = ''; });

    document.getElementById("modalmask").style.display = 'block';
    document.getElementById("logindialog").style.display = 'block';
    document.getElementById("luser").focus();
}

function hideLogin()
{
    document.getElementById("modalmask").style.display = 'none';
    document.getElementById("logindialog").style.display = 'none';
}

function loginUser(event)
{
    event.preventDefault();

    var user = document.getElementById("luser").value;
    var pass = document.getElementById("lpass").value.strip();

    var request = new XMLHttpRequest();
    request.onload = onAuthResponse;
    request.open("GET", "/game/login");
    request.setRequestHeader("Authorization", "Basic " + btoa(user + ':' + pass));
    request.send();
}

function createUser(event)
{
    event.preventDefault();

    var user = document.getElementById("cuser").value;
    var pass1 = document.getElementById("cpass1").value.strip();
    var pass2 = document.getElementById("cpass2").value.strip();

    if (/^[\w.,@+-]{3,40}$/.test(user) == false) {
        document.getElementById("cuser").value = "";
        setPrompt(
            "cprompt",
            "Your username must be between 3 and 40 characters long. Please try again.",
            true);
        return;
    }
    else if (pass1 != pass2) {
        document.getElementById("cpass1").value = "";
        document.getElementById("cpass2").value = "";
        setPrompt("cprompt", "Passwords do not match. Please try again.", true);
        return;
    }
    else if (pass1.length < 8 || pass1.length > 100) {
        document.getElementById("cpass1").value = "";
        document.getElementById("cpass2").value = "";
        setPrompt(
            "cprompt",
            "Passwords must be at between 8 and 100 characters long. Please try again.",
            true);
        return;
    }

    // Send a request to submit the proposed new user's information
    request = new XMLHttpRequest();
    request.onload = function(event) { onAuthResponse(event, true); }
    request.open("GET", "/game/create");
    request.setRequestHeader("Authorization", "Basic " + btoa(user + ':' + pass1));
    request.send();
}

function play()
{
    window.location = 'game.html';
}

function authenticate()
{
    var request = new XMLHttpRequest();
    request.onload = onAuthResponse;
    request.open("GET", "/game/auth", true);
    request.send();
}

function onAuthResponse(event, create)
{
    var request = event.target;
    var loginbar = document.getElementById("loginbar");
    var action = document.getElementById("action");
    if (request.status == 401) {
        action.className = "loginaction";
        action.value = "Login or sign up";
        action.onclick = showLogin;
        loginbar.innerHTML =
            'Welcome! <a href="javascript:void(0)" onclick="showLogin()">login or sign up</a>';
        if (create) {
            setPrompt("cprompt", "That username is already in use. Please choose another.", true);
            document.getElementById("cuser").focus();
        }
        else {
            setPrompt("cprompt", "Invalid username or password. Please try again.", true);
            document.getElementById("lpass").focus();
        }
    }
    else if (request.status == 200) {
        var data = eval('(' + request.response + ')');
        action.className = "playaction";
        action.value = "Play now";
        action.onclick = play;
        loginbar.innerHTML =
            'Welcome, ' + data["username"] + '! ' +
            '<a href="javascript:void(0)" onclick="settings()">settings</a> &middot; ' +
            '<a href="javascript:void(0)" onclick="logout()">logout</a>';

        // In case this was a login or create.
        hideLogin();
    }
    else {
        // Should indicate server problem, possibly retry later?
    }
}

function logout()
{
    var request = new XMLHttpRequest();
    request.onload = onLogoutResponse;
    request.open("GET", "/game/logout");
    request.send();
}

function onLogoutResponse(event)
{
    authenticate();
}

function init()
{
    document.getElementById("createform").onsubmit = createUser;
    document.getElementById("loginform").onsubmit = loginUser;
    authenticate();
}

document.addEventListener("DOMContentLoaded", function() { setTimeout(init, 0); });
