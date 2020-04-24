#include<Adafruit_Sensor.h>
#include <DHT.h>
#include <ESP8266WiFi.h>
#include <ThingSpeak.h>
#include <WiFiClient.h>
#include <WiFiClientSecure.h>

#define DHTPIN D5
#define PULSE_PIN D2
#define DHTTYPE DHT11
DHT dht(DHTPIN, DHTTYPE);

const int total_height = 30;
const int hold_height = 25;

int minute = 1;

const int trigger = 0;
const int echo = 3;
long Time;
int i, x;
int distanceCM, resultCM;
int tnk_lvl = 0, sensr_to_wtr = 0;

volatile long pulseCount = 0;
float calibrationFactor = 4.5;
float flowRate;
unsigned int flowMilliLitres;
unsigned long totalMilliLitres;
float totalLitres;
int m, j;

unsigned long newTime;

const char * ssid = "Redmi 4A";
const char * password = "9753124680";

WiFiClient client;

unsigned long Channel_ID = 981359;
const char * WriteAPIKey = "YJOPCAML35E4A5CJ";
const char* host = "maker.ifttt.com";
const int httpsPort = 443;
uint8_t temperature, humidity, k = 0, l = 0;

void ICACHE_RAM_ATTR pulseCounter()
{
  pulseCount++;
}

void setup() {

  Serial.begin(115200);
  pulseCount        = 0;
  flowRate          = 0.0;
  flowMilliLitres   = 0;
  totalMilliLitres  = 0;
  newTime           = 0;

  pinMode(PULSE_PIN, INPUT);

  pinMode(trigger, OUTPUT);
  pinMode(echo, INPUT);

  dht.begin();
  delay(10);

  sensr_to_wtr = total_height - hold_height;
  attachInterrupt(PULSE_PIN, pulseCounter, FALLING);

}

void loop() {

  internet();
  temperature = dht.readTemperature();
  humidity = dht.readHumidity();
  Serial.print("Temperature value is:");
  Serial.print("temperature");
  Serial.println("c");
  Serial.print("Humidity is:");
  Serial.print("humidity");
  Serial.println("%");

  for (i = 0; i < minute; i++)
  {
    Serial.println("System Standby....");
    Serial.print(i);
    Serial.println(" Minutes elapsed.");
    delay(20000);
    delay(20000);
    delay(20000);
  }
  measure();
  Serial.print("water Level:");
  Serial.print(tnk_lvl);
  Serial.println("%");


  if ((millis() - newTime) > 1000)
  {
    detachInterrupt(PULSE_PIN);
    flowRate = ((1000.0 / (millis() - newTime)) * pulseCount) / calibrationFactor;
    newTime = millis();
    flowMilliLitres = (flowRate / 60) * 1000;
    totalMilliLitres += flowMilliLitres;
    totalLitres = totalMilliLitres * 0.001;
    unsigned int frac;
    Serial.print("flowrate: ");
    Serial.print(int(flowRate));
    Serial.print(".");
    frac = (flowRate - int(flowRate)) * 10;
    Serial.print(frac, DEC) ;
    Serial.print("L/min");
    Serial.print("  Current Liquid Flowing: ");
    Serial.print(flowMilliLitres);
    j = flowMilliLitres;
    Serial.print("mL/Sec");
    Serial.print("  Output Liquid Quantity: ");
    Serial.print(totalLitres);
    Serial.println("L");
    pulseCount = 0;

    attachInterrupt(PULSE_PIN, pulseCounter, FALLING);
  }

  upload();

}

void upload() {

  internet();
  static boolean data_state = false;

  if ( data_state )
  {
    ThingSpeak.writeField(Channel_ID, 1, k, WriteAPIKey);
    data_state = false;
    upload();
  }
  else
  {
    ThingSpeak.writeField(Channel_ID, 2, l, WriteAPIKey);
    data_state = true;
    upload();
  }

  x = ThingSpeak.writeField(Channel_ID, 3, tnk_lvl, WriteAPIKey);
  if (x == 200)Serial.println("Data Updated.");
  if (x != 200)
  {
    Serial.println("Data upload failed, retrying....");
    delay(15000);
    upload();
  }

  m = ThingSpeak.writeField(Channel_ID, 4, j, WriteAPIKey);
  if (m == 200)Serial.println("Data Updated.");
  if (m != 200)
  {
    Serial.println("Data upload failed, retrying....");
    delay(15000);
    upload();
  }

}

void measure() {

  delay(100);
  digitalWrite(trigger, HIGH);
  delayMicroseconds(10);
  digitalWrite(trigger, LOW);
  Time = pulseIn(echo, HIGH);
  distanceCM = Time * 0.034;
  resultCM = distanceCM / 2;

  tnk_lvl = map(resultCM, sensr_to_wtr, total_height, 100, 0);
  if (tnk_lvl > 100) tnk_lvl = 100;
  if (tnk_lvl < 0) tnk_lvl = 0;

}

void internet() {

  Serial.println();
  Serial.println();
  Serial.print("Connecting to ");
  Serial.println(ssid);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED)
  {
    Serial.print(".");
    delay(5000);
  }
  Serial.println("\nConnected.");
  Serial.println("");
  Serial.println("WiFi connected");
  Serial.println(WiFi.localIP());
  ThingSpeak.begin(client);

  WiFiClientSecure client;
  Serial.print("connecting to ");
  Serial.println(host);
  if (!client.connect(host, httpsPort)) {
    Serial.println("connection failed");
    return;
  }

  String url = "/trigger/ESP/with/key/cml3SGwePNIdu3dPvmyw-lpK_B1wveMIh1WgVagOje0";
  Serial.print("requesting URL: ");
  Serial.println(url);

  client.print(String("GET ") + url + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" +
               "User-Agent: BuildFailureDetectorESP8266\r\n" +
               "Connection: close\r\n\r\n");

  Serial.println("request sent");
  while (client.connected()) {
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      Serial.println("headers received");
      break;
    }
  }
  String line = client.readStringUntil('\n');

  Serial.println("reply was:");
  Serial.println("==========");
  Serial.println(line);
  Serial.println("==========");
  Serial.println("closing connection");

}
