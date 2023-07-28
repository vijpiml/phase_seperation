import requests
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
import os
import time

username = input('Enter username : ')
password = input('Enter password : ')
login_times = [time.ctime()]
url = 'http://nmcheck.gnome.org'
while True:
    print('---------------------------------- Running IITJ Wifi/LAN Automatic Authentication -------------------------------------------')
    try:
        status = requests.get(url).status_code
    except:
        status = 1

    if status == 200:
        print('Login Required')
        print('Trying to Log In')
        chrome_options = Options()
        chrome_options.add_argument("--headless")
        driver = webdriver.Chrome(options=chrome_options)
        try:
            driver.get(url)
            page_source = driver.page_source
            soup = BeautifulSoup(page_source, "html.parser")
            username_field = driver.find_element(By.NAME, 'username')
            password_field = driver.find_element(By.NAME, 'password')
            submit_button = driver.find_element(By.XPATH, "//input[@type='submit' and @value='Continue']")
            username_field.clear()
            username_field.send_keys(username)
            password_field.clear()
            password_field.send_keys(password)
            submit_button.click()
            status = requests.get(url).status_code
        except:
            status = 2
        if status==200:
            print('Failed Login')
        else:
            login_times.append(time.ctime())
            with open('login_times.txt', 'w') as f:
                for i in login_times:
                    f.write(i+'\n')
            print(f'Successfully logged in as {username}')

    elif status == 1:
        print("There is no network connection, please connect to wifi or LAN then try again!!")

    elif status == 2:
        print('Trying to connect, Problem in loading login page')
        
    else:
        print('Connected!')
        print(f'Logged in as : {username}')
        print(f'Last login: {login_times[-1]}')
        
    
    time.sleep(10)
    os.system('clear')

    
