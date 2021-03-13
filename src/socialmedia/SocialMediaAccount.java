package socialmedia;

public class SocialMediaAccount {
  
  private static int count = 0;
  private int id;
  private String handle;

  public SocialMediaAccount(String handle) throws InvalidHandleException {
    if (handle.length() > 30 || handle.isBlank() || handle.contains(" ")) {
      throw new InvalidHandleException();
    }

    this.handle = handle;
    this.id = ++count;
  }

  public int getId() {
    return id;
  }

  public String getHandle() {
    return handle;
  }

  public void setId(int id) {
    this.id = id;
  }

  public void setHandle(String handle) {
    this.handle = handle;
  }
}
