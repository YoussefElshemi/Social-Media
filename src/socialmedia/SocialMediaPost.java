package socialmedia;

public class SocialMediaPost {

  private static int count = 0;
  private int id;
  private SocialMediaAccount account;
  private String message;

  public SocialMediaPost(SocialMediaAccount account, String message) throws InvalidPostException {
    if (message.isBlank() || message.length() > 100) {
      throw new InvalidPostException();
    }

    this.account = account;
    this.message = message;
    this.id = ++count;
  }

  public int getId() {
    return id;
  }

  public SocialMediaAccount getAccount() {
    return account;
  }

  public String getMessage() {
    return message;
  }

  public void setId(int id) {
    this.id = id;
  }

  public void setAccount(SocialMediaAccount account) {
    this.account = account;
  }

  public void setMessage(String message) throws InvalidPostException {
    if (message.isBlank() || message.length() > 100) {
      throw new InvalidPostException();
    }
    
    this.message = message;
  }
}
